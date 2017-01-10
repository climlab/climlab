C     path:      %P%                                                            
C     revision:  $Revision: 9.3 $                                               
C     created:   $Date: 2005/07/01 18:54:12 $                                   
C     presently: %H%  %T%                                                       
      SUBROUTINE RRTATM
C       This routine has been modified from lblatm.f 
C       (cvs revision 9.3) for use with RRTM,
C      using the translation code, lbl2r_v9.3.f
C
C                                                                               
C  --------------------------------------------------------------------------   
C |                                                                          |  
C |  Copyright 2002 - 2004, Atmospheric & Environmental Research, Inc. (AER).|  
C |  This software may be used, copied, or redistributed as long as it is    |  
C |  not sold and this copyright notice is reproduced on each copy made.     |  
C |  This model is provided as is without any express or implied warranties. |  
C |                       (http://www.rtweb.aer.com/)                        |  
C |                                                                          |  
C  --------------------------------------------------------------------------   
C                                                                               
C                                                                        FA00020
      IMPLICIT REAL*8           (V)                                     !FA00030
C                                                                        FA00040
C**********************************************************************  FA00050
C                                                                        FA00060
C     LBLATM IS AN ATMOSPHERIC RAY TRACE PROGRAM.                        FA00070
C     IT CREATES AND FORMATS THE ATMOSPHERIC INPUTS FOR THE AFGL         FA00080
C     LINE-BY-LINE TRANSMITTANCE/RADIANCE PROGRAM LBLRTM.                FA00090
C                                                                        FA00100
C     SEE THE COMMENTS IN SUBROUTINE ATMPTH FOR DETAILED INSTRUCTIONS O  FA00110
C     THE USAGE OF THE ATMOSPHERIC INPUTS.                               FA00120
C                                                                               
C     The geometry was modified for LBLRTM to reflect changes                   
C     implemented in MODTRAN to solve problems with inconsistent                
C     path parameters.                                                          
C     These changes include changing some variables and functions to            
C     double precision.                                                         
C                                                                        FA00130
C**********************************************************************  FA00140
C-                                                                       FA00150
C-                      STATEMENT FLAGS                                  FA00160
C-                                                                       FA00170
C-    LBLATM HAS BEEN STRUCTURED TO HAVE ENHANCED PORTABILITY UNDER      FA00180
C-    FORTRAN 77.  TWO FLAGS (COLUMN73) HAVE BEEN USED TO FACILITATE     FA00190
C-    PROGRAM CONVERSION.                                                FA00200
C-                                                                       FA00210
C-   &    IDENTIFIES STATEMENTS REQUIRED FOR WORD SIZE LESS THAN 8 CHAR  FA00220
C-               ALL STATEMENTS FLAGGED WITH & IN COLUMN 73 HAVE         FA00230
C-               STARTING IN COLUMN 1. THESE TWO CHARACTERS MUST         FA00240
C-               BE CHANGED TO BLANKS FOR COMPUTERS WITH WORD SIZE       FA00250
C-               LESS THAN 8 CHARACTERS.                                 FA00260
C-                                                                       FA00270
C-   !    IDENTIFIES STATEMENTS REQUIRED TO DOUBLE PRECISION THE         FA00280
C-               VARIABLES NEEDED FOR CALCULATIONS WHICH NEED MORE       FA00290
C-               THAN 32 BITS TO OBTAIN SUFFICIENT ACCURACY (I.E.        FA00300
C-               THE FREQUENCIES). STATEMENTS FLAGGED WITH ! HAVE        FA00310
C-               STARTING IN COLUMN 1. THESE TWO CHARACTERS SHOULD BE    FA00320
C-               CHANGED TO BLANKS FOR COMPUTERS HAVING SINGLE           FA00330
C-               PRECISION LESS THAN 10 SIGNIFICANT DIGITS.              FA00340
C-                                                                       FA00350
C-   >    IDENTIFIES STATEMENTS THAT MAY BE USEFUL FOR CONVERSION,       FA00360
C-               TYPICALLY SYSTEM SPECIFIC CALLS (I.E. DATE, TIME,       FA00370
C-               CPU TIME, RANDOM NUMBER, ETC.).                         FA00380
C-                                                                       FA00390
C----------------------------------------------------------------------  FA00400
C                                                                        FA00410
C     MXFSC IS THE MAXIMUM NUMBER OF LAYERS FOR OUTPUT TO LBLRTM         FA00420
C     MXLAY IS THE MAXIMUM NUMBER OF OUTPUT LAYERS                       FA00430
C     MXZMD IS THE MAX NUMBER OF LEVELS IN THE ATMOSPHERIC PROFILE       FA00440
C         STORED IN ZMDL (INPUT)                                         FA00450
C     MXPDIM IS THE MAXIMUM NUMBER OF LEVELS IN THE PROFILE ZPTH         FA00460
C         OBTAINED BY MERGING ZMDL AND ZOUT                              FA00470
C     MXMOL IS THE MAXIMUM NUMBER OF MOLECULES, KMXNOM IS THE DEFAULT    FA00480
C                                                                        FA00490
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA00500
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA00510
C                                                                        FA00520
      COMMON /PROFILE/ NLAYRS,PBAR(MXLAY),TBAR(MXLAY),
     *                 PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /MANE1/ P0,TEMP0,DVXM,H2OSLF,WTOT,ALBAR,ADBAR,AVBAR,
     *                 AVFIX,LAYRFX,SECNT0,SAMPLE,DVSET,ALFAL0,AVMASS,
     *                 DPTMIN,DPTFAC,ALTAV,AVTRAT,TDIFF1,TDIFF2,ALTD1,
     *                 ALTD2,ANGLE,IANT,LTGNT,LH1,LH2,IPFLAG,PLAY,TLAY,
     *                 EXTID(10)
      COMMON /SPECIES/ COLDRY(MXLAY),AMOUNT(MXMOL,MXLAY),WN2L(MXLAY),
     *                 CDUM(MXLAY),NMOLEC
      COMMON /PATHD1/ DVL(MXLAY),WTOTL(MXLAY),
     *                 ALBL(MXLAY),ADBL(MXLAY),AVBL(MXLAY),
     *                 H2OSL(MXLAY),IPATH(MXLAY),ITYL(MXLAY),
     *                 SECNTA(MXLAY),HT1,HT2,ALTZ(0:MXLAY)
      COMMON /XRRTATM/ IXSECT
C                                                                        FA00630
      CHARACTER*8      XID,       HMOLID,      YID                              
      Real*8               SECANT,       XALTZ                                  
C                                                                        FA00650
      COMMON /CVRATM/ HNAMATM,HVRATM
      CHARACTER*18 HNAMATM,HVRATM
      COMMON /FILHDR/ XID(10),SECANT,PAVE,TAVE,HMOLID(60),XALTZ(4),      FA00660
     *                WK(60),PZL,PZU,TZL,TZU,WN2   ,DV ,V1 ,V2 ,TBOUND,  FA00670
     *           EMISIV,FSCDID(17),nmol_flhdr,LAYER ,YI1,YID(10),LSTWDF         
C                                                                        FA00690
      EQUIVALENCE (FSCDID(3),IXSCNT) , (FSCDID(5),IEMIT)                 FA00700
C                                                                        FA00710
      CHARACTER*8      HMOLS                                            &FA00720
C                                                                        FA00730
      COMMON /HMOLS/ HMOLS(MXMOL),JUNIT(MXMOL),WMOL(MXMOL),JUNITP,       FA00740
     *               JUNITT                                              FA00750
      COMMON /HMOLC/ HMOLC(MXMOL)                                        FA00760
      CHARACTER*8 HMOLC                                                  FA00770
      character*4 ht1,ht2                                                       
C                                                                        FA00780
C     ********************************************************           FA00790
C                                                                        FA00800
C        NEW DATA FORMAT - GENERIC UNITS                                 FA00810
C                                                                        FA00820
C                                                                        FA00830
C     *********************************************************          FA00840
C                                                                        FA00850
C     IRD, IPR, IPU ARE UNIT NUMBERS FOR INPUT, OUTPUT, PUNCH            FA00860
C                                                                        FA00870
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA00880
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA00890
     *              NLTEFL,LNFIL4,LNGTH4                                 FA00900
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA00920
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                             
      COMMON /ADRIVE/ LOWFLG,IREAD,MODEL,ITYPE,NOZERO,NOP,H1F,H2F,       FA00940
     *                ANGLEF,RANGEF,BETAF,LENF,AV1,AV2,RO,IPUNCH,        FA00950
     *                XVBAR, HMINF,PHIF,IERRF,HSPACE                     FA00960
                                                                                
      COMMON /c_drive/ ref_lat,hobs,co2mx,ibmax_b,immax_b,                      
     *                 lvl_1_2,jchar_st(10,2),wm(mxzmd)                         
c                                                                               
      character*1 jchar_st                                                      
C                                                                        FA00970
      CHARACTER*8      HDATE,HTIME                                      &FA00980
C                                                                        FA00990
      COMMON /BNDRY/ ZBND(MXFSC),PBND(MXFSC),TBND(MXFSC),ALORNZ(MXFSC),  FA01000
     *               ADOPP(MXFSC),AVOIGT(MXFSC)                          FA01010
      COMMON /ZOUTP/ ZOUT(MXLAY),SOUT(MXLAY),RHOSUM(MXLAY),              FA01020
     *               AMTTOT(MXMOL),AMTCUM(MXMOL),ISKIP(MXMOL)            FA01030
C                                                                               
      CHARACTER*8      HMOD                                             &FA24840
                                                                                
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
                                                                                
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FA03670
                                                                                
                                                                                
      COMMON /CNTRL/ I1,I2,I3,I4,NBNDL,I6,I7,NBNDF,I9                    FA01040
      COMMON /PCHINF/ MUNITS,CTYPE(MXLAY)                                       
C                                                                               
      CHARACTER*3 CTYPE                                                         
C                                                                               
C     ASSIGN CVS VERSION NUMBER TO MODULE                                       
C                                                                               
      HVRATM = '$Revision: 9.3 $'                                               
C                                                                        FA01050
C     IBDIM IS THE MAXIMUM NUMBER OF LAYERS FOR OUTPUT TO LBLRTM         FA01060
C     IOUTDM IS THE MAXIMUN NUMBER OF OUTPUT LAYERS                      FA01070
C     IMDIM IS THE MAX NUMBER OF LEVELS IN THE ATMOSPHERIC PROFILE       FA01080
C         STORED IN ZMDL (INPUT)                                         FA01090
C     IPDIM IS THE MAXIMUM NUMBER OF LEVELS IN THE PROFILE ZPTH OBTAINE  FA01100
C         BY MERGING ZMDL AND ZOUT                                       FA01110
C     KDIM IS THE MAXIMUM NUMBER OF MOLECULES, KMXNOM IS THE DEFAULT     FA01120
C                                                                        FA01130
      KDIM = MXMOL                                                       FA01140
      IMDIM = MXZMD                                                      FA01150
      IOUTDM = MXLAY                                                     FA01160
      IPDIM = MXPDIM                                                     FA01170
      IBDIM = MXFSC                                                      FA01180
C                                                                        FA01190
      CALL LBLDAT(HDATE)                                                 FA01200
      CALL FTIME (HTIME)                                                 FA01210
      WRITE (IPR,900) HDATE,HTIME                                        FA01220
C                                                                        FA01230
      DO 10 M = 1, MXMOL                                                 FA01240
         READ (HMOLC(M),905) HMOLS(M)                                    FA01250
         HMOLID(M) = HMOLS(M)                                            FA01260
   10 CONTINUE                                                           FA01270
C                                                                        FA01280
      CALL ATMPTH (xid,IEMIT)                                                FA0
C                                                                        FA01300
      SECANT = 1.0                                                       FA01310
      nmol_flhdr = nmol                                                      FA0
C                                                                        FA01330
C     FOR IXSECT = 1, CALL XAMNTS                                        FA01340
C                                                                        FA01350
C     IXSECT = IXSCNT/10
      XV1 = V1-25.                                                       FA01370
      XV2 = V2+25.                                                       FA01380
      IF (IXSECT.EQ.1) CALL XAMNTS (XV1,XV2)                             FA01390
C                                                                        FA01400
      RETURN                                                             FA01410
C                                                                        FA01420
  900 FORMAT ('1',20X,'*****PROGRAM LBLATM*****     ',A10,5X,A10,///)    FA01430
  905 FORMAT (A8)                                                        FA01440
C                                                                        FA01450
      END                                                                FA01460
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE ATMPTH (xid,IEMIT)                                          FA0
C                                                                        FA01480
C**********************************************************************  FA01490
C                                                                        FA01500
C                                                                        FA01510
C                                                                        FA01520
C                                                                        FA01530
C                  ATMPTH   (ATMOSPHERIC PATH)                           FA01540
C                                                                        FA01550
C                                                                        FA01560
C                                                                        FA01570
C                                                                        FA01580
C                            WILLIAM O. GALLERY                          FA01590
C                          + GAIL   P.  ANDERSON                         FA01600
C                            FRANCIS X. KNEIZYS                          FA01610
C                            JAMES   H. CHETWYND JR.                     FA01620
C                            SHEPARD A. CLOUGH                           FA01630
C                                                                        FA01640
C                                                                        FA01650
C                           +(POINT OF CONTACT FOR THIS PROGRAM)         FA01660
C                                                                        FA01670
C                                      AIR FORCE GEOPHYSICS LAB          FA01680
C                                      OPTICAL PHYSICS DIVISION          FA01690
C                                      HANSCOM AFB                       FA01700
C                                      BEDFORD, MA.  01731               FA01710
C                                      617-861-4774                      FA01720
C                                                                        FA01730
C                                                                        FA01740
C                                      REVISED:   JULY 1990              FA01750
C                                                                        FA01760
C**********************************************************************  FA01770
C                                                                        FA01780
C                                                                        FA01790
C     USER INSTRUCTIONS:                                                 FA01800
C                                                                        FA01810
C     ATMPTH CALCULATES THE DENSITY WEIGHTED MEAN TEMPERATURE AND        FA01820
C     PRESSURE AND THE INTEGRATED ABSORBER AMOUNTS (IN MOLECULES         FA01830
C     CM-2) FOR EACH LAYER ALONG A PATH THROUGH A LAYERED                FA01840
C     ATMOSPHERE, INCLUDING THE EFFECTS OF REFRACTION AND THE  EARTH'S   FA01850
C     CURVATURE.  ATMPTH IS DESIGNED TO PREPARE THE ATMOSPHERIC INPUTS   FA01860
C     TO THE PROGRAM LBLRTM WHICH DOES A LINE-BY-LINE CALCULATION OF     FA01870
C     ATMOSPHERIC TRANSMITTANCE OR RADIANCE AND IS DESCRIBED IN          FA01880
C     REFERENCE (1).  THE CONTROL CARDS REQUIRED TO RUN ATMPTH ARE       FA01890
C     DESCRIBED LATER IN THESE COMMENTS.  A DETAILED DESCRIPTION         FA01900
C     OF THE ALGORITHM USED HERE AND A DISCUSSION OF THE EFFECTS OF      FA01910
C     THE EARTH'S CURVATURE AND REFRACTION ARE GIVEN IN REFERENCE (2).   FA01920
C                                                                        FA01930
C     THE DEFINITIONS AND USES OF THE PATH PARAMETERS ITYPE, H1, H2,     FA01940
C     ANGLE, RANGE, BETA, AND LEN ARE DESCRIBED IN REFERENCE (2) AND     FA01950
C     ARE THE SAME AS IN REFERENCE (4).                                  FA01960
C                                                                        FA01970
C     THERE ARE SIX BUILT IN ATMOSPHERIC PROFILES WHICH DEFINE THE       FA01980
C     PRESSURE, TEMPERATURE, AND MIXING RATIOS OF THE 28 MOLECULAR       FA01990
C     SPECIES INCLUDING H2O, CO2, O3, N2O, CO, CH4, AND O2 ON THE AFGL   FA02000
C     ATMOSPHERIC LINE PARAMETERS COMPILATION AT 50 STANDARD             FA02010
C     ALTITUDES.  THESE MODEL ATMOSPHERES ARE DESCRIBED IN               FA02020
C     REFERENCE (3).  THE USER MAY ALSO INPUT AN ATMOSPHERIC             FA02030
C     PROFILE AS DESCRIBED LATER (SEE ALSO THE COMMENTS IN               FA02040
C     THE SUBROUTINE NSMDL). TWENTY-0NE ADDITIONAL MIXING RATIO PROFILE  FA02050
C     FOR SPECIES CORRESPONDING TO THE MOLECULES ON THE AFGL TRACE GAS   FA02060
C     COMPILATION ARE INCLUDED.                                          FA02070
C                                                                        FA02080
C     THE PRINCIPAL OUTPUT CONSISTS OF THE INTEGRATED ABSORBER AMOUNTS   FA02090
C     FOR A SET OF LAYERS TO BE INPUT TO THE LINE-BY-LINE CALCULATION.   FA02100
C     THE NUMBER OF THESE LAYERS REPRESENTS A TRADEOFF BETWEEN ACCURACY  FA02110
C     AND COMPUTATIONAL SPEED OF THE LINE-BY-LINE CALCULATION.  THE      FA02120
C     USER HAS THE OPTION OF INPUTTING HIS OWN SET OF LAYER BOUNDARIES   FA02130
C     OR OF LETTING THE SUBROUTINE AUTLAY GENERATE THESE LAYERS          FA02140
C     AUTOMATICALLY.  IF THE USER INPUTS BOUNDARY ALTITUDES,  THEY NEED  FA02150
C     NOT FALL ON THE ATMOSPHERIC PROFILE BOUNDARIES OR INCLUDE THE      FA02160
C     PATH ENDPOINTS. IF AUTOMATIC LAYERING IS SELECTED, THE USER MAY    FA02170
C     SPECIFY THE MAXIMUM HALFWIDTH RATIO ACROSS A LAYER AND THE         FA02180
C     MAXIMUM TEMPERATURE DIFFERENCE ACROSS A LAYER.                     FA02190
C                                                                        FA02200
C     IT IS DIFFICULT TO SPECIFY APRIORI THE RELATIONSHIP BETWEEN        FA02210
C     THE NUMBER OF LAYERS AND THE ACCURACY:  THE ACCURACY DEPENDS UPON  FA02220
C     SUCH FACTORS AS THE SPECTRAL REGION, THE DISTRIBUTION OF THE       FA02230
C     MOLECULES OF INTEREST, THE PARTICULAR PATH TAKEN, AND WHETHER      FA02240
C     TRANSMITTANCE OR RADIANCE IS CALCULATED. THE LAYERING CREATED      FA02250
C     BY THE DEFAULT VALUES OF AVTRAT (1.5) AND TDIFF1 (5.0 K) AND       FA02260
C     TDIFF2 (8.0 K) SHOULD BE CONSIDERED A POINT OF DEPARTURE FOR       FA02270
C     SUBSEQUENT CALCULATIONS. THE USER SHOULD THEN EXPERIMENT WITH      FA02280
C     DIFFERENT LAYERING UNTIL THE RESULTS ARE CONSISTENT WITH           FA02290
C     HIS ACCURACY REQUIREMENTS.                                         FA02300
C                                                                        FA02310
C     TO SAVE COMPUTER TIME IN LBLRTM, THE LAYER AMOUNTS ARE ZEROED      FA02320
C     OUT WHEN                                                           FA02330
C         1.  THE CUMULATIVE AMOUNT FOR THAT LAYER AND ABOVE IS LESS     FA02340
C             THAN 0.1 PERCENT OF THE TOTAL,                             FA02350
C         AND                                                            FA02360
C         2.  A. TRANSMITTANCE IS CALCUALTED (IEMIT = 0)                 FA02370
C             OR                                                         FA02380
C             B. RADIANCE IS CALCULATED (IEMIT = 1) AND THE PATH IS      FA02390
C                LOOKING UP ( IPATH = 3)                                 FA02400
C     O2 IS  NOT CONSIDERED IN THIS SCHEME.  IF THE ABSORBER             FA02410
C     FOR A LAYER FOR ALL THE MOLECULES (EXCEPT O2) ARE ZEROED           FA02420
C     OUT, THEN THAT LAYER AND THOSE ABOVE ARE ELIMINATED                FA02430
C                                                                        FA02440
C     TO CALCULATE THE AMOUNTS FOR THE TRACE GASES (MOLECULES 8 THROUGH  FA02450
C     31) THE USER MUST INCREASE NMOL ON CARD 3.1.                       FA02460
C                                                                        FA02470
C----------------------------------------------------------------------  FA02480
C----------------------------------------------------------------------  FA02490
C                                                                        FA02500
C     OUTPUT :                                                           FA02510
C                                                                        FA02520
C     THE PRINTED OUTPUT IS ON FILE IPR (DEFAULT=6). SELECTING           FA02530
C     NOPRNT=1 SUPRESSES THE PRINTING OF THE ATMOSPHERIC PROFILES        FA02540
C     AND THE LAYER-BY-LAYER RESULTS FOR THE REFRACTED PATH.             FA02550
C     IF IPUNCH = 1, THEN THE LBLRTM INPUT DATA IS ALSO PUT ON FILE      FA02560
C     IPU (DEFAULT=7) AND CONSISTS OF A SINGLE CARD IMAGE GIVING THE     FA02570
C     NUMBER OF LAYERS LMAX AND A 70 CHARACTER FIELD DESCRIBING THE      FA02580
C     PROFILE AND THE PATH, FOLLOWED BY TWO (OR MORE) CARD IMAGES FOR    FA02590
C     EACH OF THE LMAX LAYERS                                            FA02600
C                                                                        FA02610
C        CARD 2.1    IFORM,LMAX,NMOL,SECNT0,HMOD (1X,I1,I3,I5,F10.6,3A8) FA02620
C             IFORM  = COLUMN AMOUNT FORMAT FLAG                                
C             LMAX   = NUMBER OF LBLRTM LAYERS, MAY DIFFER FROM          FA02630
C                      IBMAX DEPENDING ON THE PATH.                      FA02640
C             NMOL   = NUMBER OF MOLECULES SELECTED                      FA02650
C             SECNT0 = EFFECTIVE SECANT (SCALE FACTOR) FOR THE AMOUNTS   FA02660
C             HMOD   = 24 CHARACTER FIELD.                               FA02670
C                                                                        FA02680
C        CARD 2.1.1  PBAR(L),TBAR(L),SECNTK(L),ITYL(L),IPATH(L),         FA02690
C                    ALTZ(L-1),PZ(L-1),TZ(L-1),ALTZ(L),PZ(L),TZ(L)       FA02700
C                  (E15.7,2F10.4,A3,I2,1X,F7.2,F8.3,F7.2,F7.2,F8.3,F7.2) FA02710
C             PBAR   =  AVERAGE PRESSURE (MB)                            FA02720
C             TBAR   =  AVERAGE TEMPERATURE (K)                          FA02730
C             SECNTK = SCALE FACTOR FOR COLUMN AMOUNT (DEFAULT=0)        FA02740
C             ITYL  : OVERRIDES THE LBLRTM INTERNAL CALCULATION FOR      FA02750
C                     ITYPE, NORMALLY LEFT BLANK                         FA02760
C             IPATH : IF THE PATH DOES NOT GO THROUGH A TANGENT HEIGHT,  FA02770
C                         IF H1.LT.H2   IPATH = 3                        FA02780
C                         IF H1.GT.H2   IPATH = 1                        FA02790
C                      IF THE PATH GOES THROUGH A TANGENT HEIGHT, THEN   FA02800
C                         FOR THE LAYERS FROM THE TANGENT HEIGHT TO      FA02810
C                         MIN(H1,H2),   IPATH = 2                        FA02820
C                         FOR THE LAYERS (IF ANY) FROM MIN(H1,H2)        FA02830
C                         TO H1,  IPATH = 1                              FA02840
C                         FOR THE LAYERS (IF ANY) FROM MIN(H1,H2)        FA02850
C                         TO H2,  IPATH = 3                              FA02860
C                      FOR A HORIZONTAL PATH,  IPATH = 0                 FA02870
C             ALTZ(L)    UPPER BOUNDARY ALTITUDE (CURRENT LAYER)         FA02880
C             ALTZ(L-1)  LOWER BOUNDARY ALTITUDE (FOR FIRST LAYER ONLY)  FA02890
C             PZ(L)      PRESSURE AT ALTZ(L), MB                         FA02900
C             PZ(L-1)    PRESSURE AT ATLZ(L-1),  (FOR FIRST LAYER ONLY)  FA02910
C             TZ(L)      TEMPERATURE AT ALTZ(L), DEGREES K               FA02920
C             TZ(L-1)    TEMPERATURE AT ALTZ(L-1),(FOR FIRST LAYER ONLY  FA02930
C                                                                        FA02940
C        CARD 2.1.2           (AMOUNT(K,L),K=1,7),WBROADL(L)             FA02950
C                             (1P8E15.7)                                 FA02960
C        CARD 2.1.3           (AMOUNT(K,L),K=8,NMOL)                     FA02970
C                             (1P8E15.7)                                 FA02980
C             AMOUNT(K)   COLUMN DENSITIES FOR THE K'TH MOLECULAR        FA02990
C                         SPECIES (MOLECULES CM-2)                       FA03000
C             WBROADL(L)  COLUMN DENSITY FOR BROADENING GASES            FA03010
C                         (MOLECULES CM-2)                               FA03020
C                                                                        FA03030
C        CARDS 2.1 ARE REPEATED UNITL LMAX LAYERS ARE SPECIFIED.         FA03040
C                                                                        FA03050
C----------------------------------------------------------------------  FA03060
C                                                                        FA03070
C  REFERENCES:                                                           FA03080
C                                                                        FA03090
C (1) LBLRTM - A USERS' GUIDE (AVAILABLE FROM S.A. CLOUGH AT             FA03100
C                    THE ABOVE ADDRESS)                                  FA03110
C        SEE ALSO:                                                       FA03120
C          FASCODE - FAST ATMOSPHERIC SIGNATURE CODE                     FA03130
C          (SPECTRAL TRANSMITTANCE AND RADIANCE)                         FA03140
C                                                       AFGL-TR-78-0081  FA03150
C                                                                        FA03160
C (2) AIR MASS COMPUTER PROGRAM FOR ATMOSPHERIC TRANSMITTANCE/RADIANCE:  FA03170
C     FSCATM                                                             FA03180
C        W. O. GALLERY, F. X. KNEIZYS, AND S. A. CLOUGH                  FA03190
C                                                       AFGL-TR-83-0065  FA03200
C                                                                        FA03210
C (3) AFGL ATMOSPHERIC CONSTITUENT PROFILES (0-120 KM)                   FA03220
C        G. P. ANDERSON, S. A. CLOUGH, F.X. KNEIZYS, J. H. CHETWYND      FA03230
C        AND E. P. SHETTLE                                               FA03240
C                                                       AFGL-TR-86-0110  FA03250
C                                                                        FA03260
C (4) ATMOSPHERIC TRANSMITTANCE/RADIANCE:                                FA03270
C     COMPUTER CODE LOWTRAN 5                                            FA03280
C                                                       AFGL-TR-80-0067  FA03290
C                                                                        FA03300
C**********************************************************************  FA03310
C                                                                        FA03320
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA03330
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA03340
      PARAMETER (NXZOUT=MXLAY*3+MXMOL*3)
      PARAMETER (NX1=2*MXLAY,NX2=2*MXLAY+MXMOL*MXLAY,NX3=9*MXLAY+2)
C                                                                        FA03360
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA03370
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA03380
     *              NLTEFL,LNFIL4,LNGTH4                                 FA03390
      COMMON /MSACCT/ IOD,IDIR,ITOP,ISURF,MSPTS,MSPANL(MXLAY),           FA03400
     *                MXPNL1(MXLAY),MSLAY1,ISFILE,JSFILE,KSFILE,         FA03410
     *                LSFILE,MSFILE,IEFILE,JEFILE,KEFILE                 FA03420
      COMMON /MSCONS/ AIRMSS(MXLAY),TGRND,SEMIS(3),HMINMS,HMAXMS,        FA03430
     *                MSFLAG,MSWIT,IODFIL,MSTGLE                         FA03440
      COMMON /ADRIVE/ LOWFLG,IREAD,MODEL,ITYPE,NOZERO,NOP,H1F,H2F,       FA03450
     *                ANGLEF,RANGEF,BETAF,LENF,V1,V2,RO,IPUNCH,XVBAR,    FA03460
     *                HMINF,PHIF,IERRF,HSPACE                            FA03470
                                                                                
      COMMON /c_drive/ ref_lat,hobs,co2mx,ibmax_b,immax_b,                      
     *                 lvl_1_2,jchar_st(10,2),wm(mxzmd)                         
c                                                                               
      character*1 jchar_st                                                      
c                                                                               
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,        FA03480
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA03490
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                             
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
C                                                                        FA03530
C     BLANK COMMON FOR ZMDL                                              FA03540
C                                                                        FA03550
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA03560
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA03570
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA03580
C                                                                        FA03590
      CHARACTER*8      HMOD                                             &FA03600
C                                                                        FA03610
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),   FA03630
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),        FA03640
     *       AMTP(MXMOL,MXPDIM)                                          FA03650
C                                                                        FA03660
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FA03670
C                                                                        FA03680
      CHARACTER*8      HMOLS                                            &FA03690
C                                                                        FA03700
      COMMON /HMOLS/ HMOLS(MXMOL),JUNIT(MXMOL),WMOL(MXMOL),JUNITP,       FA03710
     *               JUNITT                                              FA03720
      COMMON /HMOLC/ HMOLC(MXMOL)                                        FA03730
      CHARACTER*8 HMOLC                                                  FA03740
C                                                                        FA03750
C     ********************************************************           FA03760
C                                                                        FA03770
C        NEW DATA FORMAT : GENERIC UNITS                                 FA03780
C                                                                        FA03790
C                                                                        FA03800
C     *********************************************************          FA03810
C                                                                        FA03820
      COMMON /PROFILE/ NLAYRS,PBAR(MXLAY),TBAR(MXLAY),
     *                 PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /MANE1/ P0,TEMP0,DVXM,H2OSLF,WTOT,ALBAR,ADBAR,AVBAR,
     *                 AVFIX,LAYRFX,SECNT0,SAMPLE,DVSET,ALFAL0,AVMASS,
     *                 DPTMIN,DPTFAC,ALTAV,AVTRAT,TDIFF1,TDIFF2,ALTD1,
     *                 ALTD2,ANGLE,IANT,LTGNT,LH1,LH2,IPFLAG,PLAY,TLAY,
     *                 EXTID(10)
      COMMON /SPECIES/ COLDRY(MXLAY),AMOUNT(MXMOL,MXLAY),WN2L(MXLAY),
     *                 CDUM(MXLAY),NMOLEC
      COMMON /PATHD1/ DVL(MXLAY),WTOTL(MXLAY),
     *                 ALBL(MXLAY),ADBL(MXLAY),AVBL(MXLAY),
     *                 H2OSL(MXLAY),IPATH(MXLAY),ITYL(MXLAY),
     *                 SECNTA(MXLAY),HT1,HT2,ALTZ(0:MXLAY)
      COMMON /BNDRY/ ZBND(MXFSC),PBND(MXFSC),TBND(MXFSC),ALORNZ(MXFSC),  FA03930
     *              ADOPP(MXFSC),AVOIGT(MXFSC)                           FA03940
      COMMON /ZOUTP/ ZOUT(MXLAY),SOUT(MXLAY),RHOSUM(MXLAY),              FA03950
     *               AMTTOT(MXMOL),AMTCUM(MXMOL),ISKIP(MXMOL)            FA03960
      COMMON /PCHINF/ MUNITS,CTYPE(MXLAY)                                       
      COMMON /FIXITYL/ IFXTYP                                                   
      DIMENSION XZOUT(NXZOUT),WMT(MXMOL)
      DIMENSION X1(NX1),X2(NX2),X3(NX3)
      DIMENSION TTMP(2),WVTMP(2),PTMP(2),ZTMP(2)                                
                                                                                
c common block for layer-to-level analytical jacobians                          
      common /dlaydlev/ilevdq,imoldq,iupdwn,                                    
     &    dqdL(mxlay,0:mxmol),dqdU(mxlay,0:mxmol)                               
      COMMON /IADFLG/ IANDER,NSPCRT,IMRGSAV                                     
      dimension densave(mxzmd)                                                  
C                                                                        FA03980
      EQUIVALENCE (ZOUT(1),XZOUT(1))
      EQUIVALENCE (PBAR(1),X1(1)),(COLDRY(1),X2(1)),(DVL(1),X3(1))
C                                                                        FA04000
      character*8 xid(10)                                                       
      CHARACTER*48 CFORM1,CFORM2                                         FA04010
      CHARACTER*8 COTHER                                                 FA04020
      CHARACTER*7 PAFORM(2)                                              FA04030
      CHARACTER*4 HT1HRZ,HT2HRZ,HT1SLT,HT2SLT,PZFORM(5),  ht1,ht2               
      CHARACTER*3 CTYPE                                                         
C                                                                        FA04050
      DATA COTHER / 'OTHER   '/                                          FA04060
      DATA AVRATS / 1.5 /,TDIF1S / 5.0 /,TDIF2S / 8.0 /                  FA04070
      DATA HT1HRZ / ' AT '/,HT2HRZ / ' KM '/,HT1SLT / ' TO '/,           FA04080
     *     HT2SLT / ' KM '/                                              FA04090
      DATA PZFORM / 'F8.6','F8.5','F8.4','F8.3','F8.2'/                  FA04100
      DATA PAFORM / '1PE15.7','  G15.7'/                                 FA04110
      DATA CFORM1 / '(1PE15.7,0PF10.2,10X,A3,I2,1X,2(F7.3,F8.3,F7.2))'/  FA04120
      DATA CFORM2 / '(  G15.7,0PF10.2,10X,A3,I2,23X,(F7.3,F8.3,F7.2))'/  FA04130
      DATA IERROR / 0 /, IPASS / 0 /                                            
      DATA T296 /296.0/                                                         
C                                                                        FA04150
C     IAMT = 1: CALCULATE AMOUNTS, IAMT = 2: DO NOT CALCULATE AMOUNTS    FA04160
C                                                                        FA04170
      DATA IAMT / 1 /                                                    FA04180
C                                                                        FA04190
C     AIRMS1 IS ONE AIRMASS OR THE TOTAL AMOUNT FOR A VERTICAL PATH      FA04200
C     FROM GROUND TO SPACE                                               FA04210
C                                                                        FA04220
      DATA AIRMS1 / 2.153E25 /                                           FA04230
      SECNT0 = 1.0                                                       FA04240
c                                                                               
      DEG = 180.0/PI                                                     FA04260
C                                                                        FA04270
C     GCAIR IS THE GAS CONSTANT FOR RHO IN MOL CM(-3), P IN MB, AND      FA04280
C     T IN K                                                             FA04290
C                                                                        FA04300
      GCAIR = 1.0E-3*GASCON/AVOGAD                                       FA04310
C                                                                        FA04320
C     ADCON IS THE CONSTANT FOR THE DOPPLER HALFWIDTH                    FA04330
C                                                                        FA04340
      ADCON = SQRT(2.0* LOG(2.0)*GASCON/CLIGHT**2)                       FA04350
C                                                                        FA04360
C     ZERO OUT COMMON BLOCKS                                             FA04370
C                                                                        FA04380
      DO 10 N = 1, MXMOL                                                 FA04390
         WMT(N) = 0.0                                                    FA04400
   10 CONTINUE                                                           FA04410
C                                                                        FA04420
C       COMMON /DEAMT/ DRYAIR with BLANK COMMON RFNDXM                   FA04430
C                                                                        FA04440
      DO 20 N = 1, IMDIM                                                 FA04450
         DRYAIR(N) = 0.                                                         
         RFNDXM(N) = 0.                                                  FA04460
   20 CONTINUE                                                           FA04470
      DO 30 N = 1, IPDIM                                                 FA04480
         IF (N.LE.IPDIM-2) THEN                                          FA04490
            ZPTH(N) = 0.                                                 FA04500
            PP(N) = 0.                                                   FA04510
            TP(N) = 0.                                                   FA04520
            RFNDXP(N) = 0.                                               FA04530
            SP(N) = 0.                                                   FA04540
            PPSUM(N) = 0.                                                FA04550
            TPSUM(N) = 0.                                                FA04560
            RHOPSM(N) = 0.                                               FA04570
         ENDIF                                                           FA04580
C                                                                        FA04590
C     COMMON /DEAMT/ DENP  with BLANK COMMON AMTP                        FA04600
C                                                                        FA04610
         DO 28 M = 1, KDIM                                               FA04620
            DENP(M,N) = 0.                                               FA04630
            AMTP(M,N) = 0.                                               FA04640
 28      CONTINUE                                                               
 30   CONTINUE                                                           FA04650
C                                                                        FA04660
C     /PROFILE/, /SPECIES/, & /PATHD1/
C                                                                        FA04680
      DO 41 L=1,NX1
         X1(L) = 0.0
   41 CONTINUE
      DO 42 L=1,NX2
         X2(L) = 0.0
   42 CONTINUE
      DO 43 L=1,NX3
         X3(L) = 0.0
   43 CONTINUE
C                                                                        FA04720
C     /ZOUT/                                                             FA04730
C                                                                        FA04740
      DO 50 N = 1, NXZOUT                                                FA04750
         XZOUT(N) = 0.0                                                  FA04760
   50 CONTINUE                                                           FA04770
C                                                                        FA04780
      IF (IREAD.LE.0) THEN                                               FA04790
C                                                                        FA04800
C     READ CONTROL CARD 3.1                                              FA04810
C                                                                        FA04820
         READ (IRD,900) MODEL,ITYPE,IBMAX_B,NOZERO,NOPRNT,NMOL,IPUNCH,   FA04830
     *                  IFXTYP,MUNITS,RE,HSPACE,XVBAR,CO2MX,REF_LAT      FA04840
         XVBAR = 1.0
         ITYPE = 2
         NOZERO = 1
         IBMAX = ABS(IBMAX_B)                                                   
      ENDIF                                                              FA04850
C                                                                        FA04860
      NOP = NOPRNT                                                       FA04870
      RO = RE                                                            FA04880
                                                                                
      IF (NOPRNT.GE.0) THEN                                                     
         WRITE (IPR,902)                                                    FA04
         WRITE (IPR,904) MODEL,ITYPE,IBMAX,NOZERO,NOPRNT,NMOL,IPUNCH,       FA04
     *                   IFXTYP,MUNITS,RE,HSPACE,XVBAR,CO2MX,REF_LAT        FA04
      ENDIF                                                                     
c                                                                               
      M = MODEL                                                          FA04940
      IF (NMOL.EQ.0) NMOL = KMXNOM                                       FA04950
      IF (ITYPE.LT.1.OR.ITYPE.GT.3) GO TO 290                            FA04960
      IF (M.LT.0.OR.M.GT.6) GO TO 290                                    FA04970
      IF (IBMAX.GT.IBDIM) GO TO 290                                      FA04980
      IF (NMOL.GT.KDIM) GO TO 290                                        FA04990
      IF (IPUNCH.EQ.1) then                                                     
c        Tape7 only opened if ipu selected and first time through               
         if (ipass.eq.0)                                                        
     *          OPEN (UNIT=IPU,FILE='TAPE7',STATUS='UNKNOWN')                   
        ipass = ipass + 1                                                       
      if (ipunch .eq. 1)                                                        
     *  write (ipu,905) ipass, xid                                              
      endif                                                                     
c                                                                               
      IF (RE.NE.0.0) GO TO 60                                            FA05010
      RE = 6371.23                                                       FA05020
      IF (M.EQ.1) RE = 6378.39                                           FA05030
      IF (M.EQ.4.OR.M.EQ.5) RE = 6356.91                                 FA05040
      RO = RE                                                            FA05050
   60 CONTINUE                                                           FA05060
C                                                                        FA05070
      IF (HSPACE.EQ.0.) HSPACE = 100.                                    FA05080
      IF (XVBAR.LE.0.) THEN                                              FA05090
         XVBAR = (V1+V2)/2.                                              FA05100
         IF (V2.LT.V1) XVBAR = V1                                        FA05110
      ENDIF                                                              FA05120
      if (REF_LAT .eq.0) REF_LAT = 45.                                          
                                                                                
C                                                                        FA05130
      IF (NOPRNT.GE.0) THEN                                                     
         WRITE (IPR,906)                                                        
         WRITE (IPR,904) MODEL,ITYPE,IBMAX,NOZERO,NOPRNT,NMOL,IPUNCH,       FA05
     *                   IFXTYP,MUNITS,RE,HSPACE,XVBAR,CO2MX,REF_LAT        FA05
      ENDIF                                                                     
C                                                                        FA05170
      IF (ITYPE.EQ.1) THEN                                               FA05180
C                                                                        FA05190
C                                                                        FA05200
C     =>   HORIZONTAL PATH SELECTED                                      FA05210
C                                                                        FA05220
C                                                                        FA05230
         IF (NOPRNT.GE.0) WRITE (IPR,908)                                       
                                                                                
C                                                                        FA05250
C        READ IN CONTROL CARD 3.2                                        FA05260
C                                                                        FA05270
         IF (IREAD.LE.0) READ (IRD,910) H1F,RANGEF                       FA05280
         RANGE = RANGEF                                                  FA05290
         ZH = H1F                                                        FA05300
         H1 = ZH                                                         FA05310
         H2 = 0.                                                         FA05320
         H2F = H2                                                        FA05330
         ANGLE = 0.                                                      FA05340
         ANGLEF = ANGLE                                                  FA05350
         BETA = 0.                                                       FA05360
         BETAF = BETA                                                    FA05370
         LEN = 0                                                         FA05380
         LENF = LEN                                                      FA05390
         IF (NOPRNT.GE.0)  WRITE (IPR,912) ZH,RANGE                             
C                                                                        FA05410
C        SET UP THE ATMOSPHERIC PROFILE                                  FA05420
C                                                                        FA05430
         CALL MDLATM (ITYPE,M,IREAD,HSPACE)                                     
C                                                                        FA05450
         IF (IMMAX.EQ.1) THEN                                            FA05460
            ZH = ZMDL(1)                                                 FA05470
            H1F = ZH                                                     FA05480
            H1 = ZH                                                      FA05490
            PH = PM(1)                                                   FA05500
            TH = TM(1)                                                   FA05510
            RHOBAR = ALOSMT*PH*TZERO/(PZERO*TH)                          FA05520
            DO 70 K = 1, NMOL                                            FA05530
               DENP(K,1) = DENM(K,1)                                     FA05540
   70       CONTINUE                                                     FA05550
            DENW(1) = DENM(1,1)                                          FA05560
            GO TO 110                                                    FA05570
         ENDIF                                                           FA05580
C                                                                        FA05590
C        INTERPOLATE ATMOSPHERIC PROFILE DENSITIES TO ZH                 FA05600
C                                                                        FA05610
         DO 80 IM = 2, IMMAX                                             FA05620
            IF (ZH.LT.ZMDL(IM)) GO TO 90                                 FA05630
   80    CONTINUE                                                        FA05640
         IM = IMMAX                                                      FA05650
   90    CONTINUE                                                        FA05660
         A = (ZH-ZMDL(IM-1))/(ZMDL(IM)-ZMDL(IM-1))                       FA05670
         CALL EXPINT (PH,PM(IM-1),PM(IM),A)                              FA05680
         TH = TM(IM-1)+(TM(IM)-TM(IM-1))*A                               FA05690
         RHOBAR = ALOSMT*PH*TZERO/(PZERO*TH)                             FA05700
         DO 100 K = 1, NMOL                                              FA05710
            CALL EXPINT (DENP(K,1),DENM(K,IM-1),DENM(K,IM),A)            FA05720
  100    CONTINUE                                                        FA05730
C                                                                        FA05740
  110    CONTINUE                                                               
         IF (NOPRNT.GE.0) THEN                                                  
            WRITE (IPR,914) HMOD,ZH,PH,TH,(HMOLS(K),K=1,NMOL)               FA05
            WRITE (IPR,916) RHOBAR,(DENP(K,1),K=1,NMOL)                     FA05
         ENDIF                                                                  
C                                                                        FA05770
C        COMPUTE AMOUNTS FOR A HORIZONTAL PATH                           FA05780
C                                                                        FA05790
         DO 120 K = 1, NMOL                                              FA05800
            AMOUNT(K,1) = DENP(K,1)*RANGE*1.0E+5                         FA05810
  120    CONTINUE                                                        FA05820
         AMTAIR = RHOBAR*RANGE*1.0E+5                                    FA05830
         IF (NOPRNT.GE.0) THEN                                                  
            WRITE (IPR,918) HMOD,ZH,PH,TH,RANGE,(HMOLS(K),K=1,NMOL)         FA05
            WRITE (IPR,920) AMTAIR,(AMOUNT(K,1),K=1,NMOL)                   FA05
         ENDIF                                                                  
         IPATH(1) = 0                                                    FA05860
         LMAX = 1                                                        FA05870
         NLAYRS = 1                                                      FA05880
C                                                                        FA05890
         SUMAMT = 0.                                                     FA05900
         DO 130 K = 1, NMOL                                              FA05910
            SUMAMT = SUMAMT+AMOUNT(K,1)                                  FA05920
  130    CONTINUE                                                        FA05930
         WN2L(1) = AMTAIR-SUMAMT                                         FA05940
C                                                                        FA05950
         PBAR(1) = PH                                                    FA05960
         TBAR(1) = TH                                                    FA05970
         ALTZ(0) = -RANGE                                                FA05980
         ZOUT(1) = ZH                                                    FA05990
         IOUTMX = 1                                                      FA06000
         SECNTA(1) = 1.                                                  FA06010
         ALTZ(1) = ZH                                                    FA06020
         ht1 = ht1hrz                                                           
         ht2 = ht2hrz                                                           
C                                                                        FA06050
C        > Write atmosphere to TAPE7 (in E15.7 format) <                 FA06060
C                                                                        FA06070
         IF (IPUNCH.EQ.1) THEN                                           FA06080
           IFORM = 1                                                            
           WRITE (IPU,924) IFORM,LMAX,NMOL,SECNT0,HMOD,RANGE,ZH          FA06090
C                                                                               
C           -------------------------------------                               
C           > Write molecular information in    <                               
C           >  - mixing ratio if MUNITS is 1    <                               
C           >  - column density if MUNITS is 0  <                               
C           -------------------------------------                               
C                                                                               
           IF (MUNITS.EQ.1) THEN                                         FA06140
              DRAIR =  WN2L(1)                                           FA06150
              DO 135 M = 2,NMOL                                                 
                 DRAIR = DRAIR + AMOUNT(M,1)                                    
 135          CONTINUE                                                          
C                                                                               
C             > If DRAIR is zero, then write out AMOUNT only    <               
C             > (since AMOUNT zero => mixing ratio zero)        <               
C                                                                               
              IF (DRAIR.EQ.0 .AND. NOPRNT.GE.0) THEN                            
                 WRITE (IPU,926) PH,TH,IPATH(1),ZH,ZH,                          
     *                           (AMOUNT(K,1),K=1,7),WN2L(1),                   
     *                           (AMOUNT(K,1),K=8,NMOL)                         
              ELSEIF (NOPRNT.GE.0) THEN                                         
                 WRITE (IPU,926) PH,TH,IPATH(1),ZH,ZH,                   FA06160
     *                           (AMOUNT(K,1)/DRAIR,K=1,7),WN2L(1),      FA06170
     *                           (AMOUNT(K,1)/DRAIR,K=8,NMOL)            FA06180
              ENDIF                                                             
           ELSE                                                          FA06190
C                                                                               
C             Test to make sure there are no fractional molecular               
C             amounts written out (will cause PATH to assume                    
C             mixing ratio)                                                     
C                                                                               
              DO 137 K=1,NMOL                                                   
                 IF (AMOUNT(K,1).LT.1.) THEN                                    
                    IF (NOPRNT.GE.0) WRITE(IPR,1000) K,AMOUNT(K,1)              
                    AMOUNT(K,1) = 0.0                                           
                 ENDIF                                                          
 137          CONTINUE                                                          
C                                                                               
              IF (NOPRNT.GE.0)                                                  
     *             WRITE (IPU,926) PH,TH,IPATH(1),ZH,ZH,                 FA06240
     *                            (AMOUNT(K,1),K=1,7),WN2L(1),                  
     *                            (AMOUNT(K,1),K=8,NMOL)                        
           ENDIF                                                         FA06270
        ENDIF                                                            FA06280
C                                                                        FA06770
      ELSE                                                               FA06780
C                                                                        FA06790
C                                                                        FA06800
C     =>   SLANT PATH SELECTED                                           FA06810
C                                                                        FA06820
C                                                                        FA06830
C        ITYPE = 2 OR 3: SLANT PATH THROUGH THE ATMOSPHERE               FA06840
C                                                                        FA06850
         IF (NOPRNT.GE.0)   WRITE (IPR,930) ITYPE                               
C                                                                        FA06870
C      >  READ IN CONTROL CARD 3.2 CONTAINING SLANT PATH PARAMETERS <    FA06880
C                                                                        FA06890
         IF (IREAD.LE.0)                                                 FA06900
     *         READ (IRD,932) H1F,H2F,ANGLEF,RANGEF,BETAF,LENF,HOBS      FA06910
         ANGLEF = 0.
         H1 = H1F                                                        FA06920
         H2 = H2F                                                        FA06930
         ANGLE = ANGLEF                                                  FA06940
         RANGE = RANGEF                                                  FA06950
         BETA = BETAF                                                    FA06960
         LEN = LENF                                                      FA06970
         IF (NOPRNT.GE.0) THEN                                                  
            IF (IBMAX_B .LT. 0) THEN                                            
               WRITE (IPR,933) H1,H2,ANGLE,RANGE,BETA,LEN                       
            ELSE                                                                
               WRITE (IPR,934) H1,H2,ANGLE,RANGE,BETA,LEN                   FA06
            ENDIF                                                               
         ENDIF                                                                  
C                                                                        FA06990
C        > GENERATE OR READ IN LBLRTM BOUNDARY LAYERS <                  FA07000
C                                                                        FA07010
         IF (IBMAX.EQ.0) THEN                                            FA07020
C                                                                        FA07030
C           > SELECT AUTOMATIC LAYERING <                                FA07040
C                                                                        FA07050
            IF (IREAD.LE.0) THEN                                         FA07060
               READ (IRD,936) AVTRAT,TDIFF1,TDIFF2,ALTD1,ALTD2           FA07070
               IF (AVTRAT.EQ.0.0) AVTRAT = AVRATS                        FA07080
               IF (TDIFF1.EQ.0.0) TDIFF1 = TDIF1S                        FA07090
               IF (TDIFF2.EQ.0.0) TDIFF2 = TDIF2S                        FA07100
               IF ((ALTD2.LE.0).OR.(ALTD2.LE.ALTD1)) THEN                FA07110
                  ALTD1 = 0.                                             FA07120
                  ALTD2 = 100.                                           FA07130
               ENDIF                                                     FA07140
               IF (NOPRNT.GE.0)                                                 
     *              WRITE (IPR,938) AVTRAT,TDIFF1,TDIFF2,ALTD1,ALTD2          FA
               IF (AVTRAT.LE.1.0.OR.TDIFF1.LE.0.0.OR.TDIFF2.LE.0.0)      FA07160
     *              GO TO 320                                            FA07170
            ENDIF                                                        FA07180
            GO TO 150                                                    FA07190
C                                                                        FA07200
         ENDIF                                                           FA07210
C                                                                        FA07220
C        > READ IN LBLRTM BOUNDARY LAYERS <                              FA07230
C                                                                        FA07240
         IF (IREAD.LE.0) THEN                                            FA07250
            IF (IBMAX_B .LT. 0) THEN                                            
               READ (IRD,940) (PBND(IB),IB=1,IBMAX)                             
               IF (NOPRNT.GE.0)                                                 
     *              WRITE (IPR,943) (IB,PBND(IB),IB=1,IBMAX)                    
            ELSE                                                                
               READ (IRD,940) (ZBND(IB),IB=1,IBMAX)                      FA07260
               IF (NOPRNT.GE.0)                                                 
     *              WRITE (IPR,942) (IB,ZBND(IB),IB=1,IBMAX)                  FA
            ENDIF                                                               
         ENDIF                                                           FA07280
C                                                                        FA07290
         IF (IBMAX.EQ.0) GO TO 150                                       FA07300
                                                                                
         IF (IBMAX_B .GT. 0) THEN                                               
            DO 140 IB = 2, IBMAX                                         FA07310
               IF (ZBND(IB).LE.ZBND(IB-1)) GO TO 300                     FA07320
  140       CONTINUE                                                     FA07330
         ENDIF                                                                  
         IF (IBMAX_B .LT. 0) THEN                                               
           DO 145 IB = 2,IBMAX                                                  
               IF (PBND(IB) .GE. PBND(IB-1)) GO TO 305                          
  145        CONTINUE                                                           
         ENDIF                                                                  
 150    CONTINUE                                                        FA07340 
                                                                                
C                                                                        FA07350
C        > SET UP ATMOSPHERIC PROFILE <                                  FA07360
C                                                                        FA07370
                                                                                
         CALL MDLATM (ITYPE,M,IREAD,HSPACE)                                     
                                                                                
                                                                                
C INTERPOLATE PBND GRID ONTO ZBND GRID.                                         
                                                                                
C TO ENSURE THAT CALCULATED/INPUT ZMDL'S WILL MATCH CALCULATED USER-LEVEL       
C ALTITUDES, A COMBINATION OF INTERPOLATION AND HYDROSTATICS ARE USED.          
C ZBND = A * F1(P) + (1 - A) * F2(P), WHERE                                     
C F1(P) = INTERPOLATION IN LN(P), F2(P) = HYDROSTATIC CALCULATION               
                                                                                
         IF (IBMAX_B .LT. 0) THEN                                               
                                                                                
            ISTART = 2                                                          
                                                                                
            DO 160 IP=1,IBMAX                                                   
               PTMP(1) = 0.0                                                    
               TTMP(1) = 0.0                                                    
               WVTMP(1) = 0.0                                                   
               ZTMP(1) = 0.0                                                    
                                                                                
               PTMP(2) = 0.0                                                    
               TTMP(2) = 0.0                                                    
               WVTMP(2) = 0.0                                                   
               ZTMP(2) = 0.0                                                    
                                                                                
               DO 161 LIP=ISTART,IMMAX                                          
                  IF (PBND(IP) .GT. PM(LIP)) GO TO 162                          
 161              CONTINUE                                                      
                  LIP=IMMAX                                                     
 162              CONTINUE                                                      
                  IF (PBND(IP) .EQ. PM(LIP-1)) THEN                             
                     ZBND(IP) = ZMDL(LIP-1)                                     
                     TBND(IP) = TM(LIP-1)                                       
                  ELSE                                                          
                     IF(PBND(IP) .EQ. PM(LIP)) THEN                             
                        ZBND(IP) = ZMDL(LIP)                                    
                        TBND(IP) = TM(LIP)                                      
                     ELSE                                                       
                                                                                
C PERFORM INTERPOLATION IN LN(PM)                                               
                        HIP =  (ZMDL(LIP)-ZMDL(LIP-1))/                         
     &                        LOG(PM(LIP)/PM(LIP-1))                            
                        ZINT = ZMDL(LIP-1)+                                     
     &                       HIP* LOG(PBND(IP)/PM(LIP-1))                       
                                                                                
C PERFORM ALTITUDE CALCULATION USING HYDROSTATIC EQUATION                       
                        PTMP(1) = PM(LIP-1)                                     
                        ZTMP(1) = ZMDL(LIP-1)                                   
                        TTMP(1) = TM(LIP-1)                                     
                        WVTMP(1) = DENW(LIP-1)                                  
                                                                                
                        PTMP(2) = PBND(IP)                                      
                                                                                
                        TIP = (TM(LIP)-TM(LIP-1))/                              
     &                        LOG(PM(LIP)/PM(LIP-1))                            
                        TTMP(2) = TM(LIP-1)+                                    
     &                       TIP* LOG(PBND(IP)/PM(LIP-1))                       
                                                                                
                        WVIP =  (DENW(LIP)-DENW(LIP-1))/                        
     &                        LOG(PM(LIP)/PM(LIP-1))                            
                        WVTMP(2) =  DENW(LIP-1) +                               
     &                       WVIP* LOG(PBND(IP)/PM(LIP-1))                      
                        CALL CMPALT(2,PTMP,TTMP,                                
     &                       WVTMP,ZTMP(1),REF_LAT,ZTMP)                        
                                                                                
C COMBINE THE INTERPOLATION AND THE HYDROSTATIC CALCULATION                     
                                                                                
                        RATP =  LOG(PBND(IP)/PM(LIP-1))/                        
     &                        LOG(PM(LIP)/PM(LIP-1))                            
                                                                                
                        A = RATP**3                                             
                                                                                
                        ZBND(IP) = A*ZINT + (1-A)*ZTMP(2)                       
                        TBND(IP) = TTMP(2)                                      
                     ENDIF                                                      
               ENDIF                                                            
                                                                                
               ISTART = LIP                                                     
                                                                                
 160           CONTINUE                                                         
                                                                                
C INTERPOLATE H1, H2 ONTO ALTITUDE GRID                                         
            PTMP(1) = 0.0                                                       
            TTMP(1) = 0.0                                                       
            WVTMP(1) = 0.0                                                      
            ZTMP(1) = 0.0                                                       
                                                                                
            PTMP(2) = 0.0                                                       
            TTMP(2) = 0.0                                                       
            WVTMP(2) = 0.0                                                      
            ZTMP(2) = 0.0                                                       
                                                                                
            DO 166 LIP = 2,IMMAX                                                
               IF (H1 .GT. PM(LIP)) GO TO 167                                   
 166           CONTINUE                                                         
               LIP = IMMAX                                                      
 167        CONTINUE                                                            
            IF (H1 .EQ. PM(LIP-1)) THEN                                         
               H1 = ZMDL(LIP-1)                                                 
            ELSE                                                                
               IF(H1 .EQ. PM(LIP)) THEN                                         
                  H1 = ZMDL(LIP)                                                
               ELSE                                                             
                                                                                
C PERFORM INTERPOLATION IN LN(PM)                                               
                  HIP =  (ZMDL(LIP)-ZMDL(LIP-1))/                               
     &                  LOG(PM(LIP)/PM(LIP-1))                                  
                  ZINT = ZMDL(LIP-1)+                                           
     &                 HIP* LOG(H1/PM(LIP-1))                                   
                                                                                
C PERFORM ALTITUDE CALCULATION USING HYDROSTATIC EQUATION                       
                  PTMP(1) = PM(LIP-1)                                           
                  ZTMP(1) = ZMDL(LIP-1)                                         
                  TTMP(1) = TM(LIP-1)                                           
                  WVTMP(1) = DENW(LIP-1)                                        
                                                                                
                  PTMP(2) = H1                                                  
                                                                                
                  TIP = (TM(LIP)-TM(LIP-1))/                                    
     &                  LOG(PM(LIP)/PM(LIP-1))                                  
                  TTMP(2) = TM(LIP-1)+                                          
     &                 TIP* LOG(H1/PM(LIP-1))                                   
                                                                                
                  WVIP =  (DENW(LIP)-DENW(LIP-1))/                              
     &                  LOG(PM(LIP)/PM(LIP-1))                                  
                  WVTMP(2) =  DENW(LIP-1) +                                     
     &                 WVIP* LOG(H1/PM(LIP-1))                                  
                  CALL CMPALT(2,PTMP,TTMP,                                      
     &                 WVTMP,ZTMP(1),REF_LAT,ZTMP)                              
                                                                                
C COMBINE THE INTERPOLATION AND THE HYDROSTATIC CALCULATION                     
                                                                                
                  RATP =  LOG(H1/PM(LIP-1))/                                    
     &                        LOG(PM(LIP)/PM(LIP-1))                            
                                                                                
                  A = RATP**3                                                   
                                                                                
                  H1 = A*ZINT + (1-A)*ZTMP(2)                                   
               ENDIF                                                            
            ENDIF                                                               
                                                                                
            IF (H1 .LT. 0.0) THEN                                               
               PRINT 946, H1,ZTMP(1)                                            
               IF (NOPRNT.GE.0) WRITE (IPR,946) H1,ZTMP(1)                      
               STOP ' COMPUTED ALTITUDE VALUE OF H1 IS NEGATIVE'                
            ENDIF                                                               
                                                                                
            PTMP(1) = 0.0                                                       
            TTMP(1) = 0.0                                                       
            WVTMP(1) = 0.0                                                      
            ZTMP(1) = 0.0                                                       
                                                                                
            PTMP(2) = 0.0                                                       
            TTMP(2) = 0.0                                                       
            WVTMP(2) = 0.0                                                      
            ZTMP(2) = 0.0                                                       
                                                                                
            DO 168 LIP = 2,IMMAX                                                
               IF (H2 .GT. PM(LIP)) GO TO 169                                   
 168           CONTINUE                                                         
               LIP = IMMAX                                                      
 169        CONTINUE                                                            
            IF (H2 .EQ. PM(LIP-1)) THEN                                         
               H2 = ZMDL(LIP-1)                                                 
            ELSE                                                                
               IF(H2 .EQ. PM(LIP)) THEN                                         
                  H2 = ZMDL(LIP)                                                
               ELSE                                                             
C PERFORM INTERPOLATION IN LN(PM)                                               
                  HIP =  (ZMDL(LIP)-ZMDL(LIP-1))/                               
     &                  LOG(PM(LIP)/PM(LIP-1))                                  
                  ZINT = ZMDL(LIP-1)+                                           
     &                 HIP* LOG(H2/PM(LIP-1))                                   
                                                                                
C PERFORM ALTITUDE CALCULATION USING HYDROSTATIC EQUATION                       
                  PTMP(1) = PM(LIP-1)                                           
                  ZTMP(1) = ZMDL(LIP-1)                                         
                  TTMP(1) = TM(LIP-1)                                           
                  WVTMP(1) = DENW(LIP-1)                                        
                                                                                
                  PTMP(2) = H2                                                  
                                                                                
                  TIP = (TM(LIP)-TM(LIP-1))/                                    
     &                  LOG(PM(LIP)/PM(LIP-1))                                  
                  TTMP(2) = TM(LIP-1)+                                          
     &                 TIP* LOG(H2/PM(LIP-1))                                   
                                                                                
                  WVIP =  (DENW(LIP)-DENW(LIP-1))/                              
     &                  LOG(PM(LIP)/PM(LIP-1))                                  
                  WVTMP(2) =  DENW(LIP-1) +                                     
     &                 WVIP* LOG(H2/PM(LIP-1))                                  
                  CALL CMPALT(2,PTMP,TTMP,                                      
     &                 WVTMP,ZTMP(1),REF_LAT,ZTMP)                              
                                                                                
C COMBINE THE INTERPOLATION AND THE HYDROSTATIC CALCULATION                     
                                                                                
                  RATP =  LOG(H2/PM(LIP-1))/                                    
     &                        LOG(PM(LIP)/PM(LIP-1))                            
                                                                                
                  A = RATP**3                                                   
                                                                                
                  H2 = A*ZINT + (1-A)*ZTMP(2)                                   
               ENDIF                                                            
            ENDIF                                                               
                                                                                
            IF (H2 .LT. 0.0) THEN                                               
               PRINT 946, H2,ZTMP(1)                                            
               IF (NOPRNT.GE.0) WRITE (IPR,946) H2,ZTMP(1)                      
               STOP ' COMPUTED ALTITUDE VALUE OF H2 IS NEGATIVE'                
            ENDIF                                                               
         ENDIF                                                                  
                                                                                
         IERB = 0                                                               
         IF (IBMAX.GE.1) THEN                                            FA07400
            IF (ZBND(1).LT.ZMDL(1)) THEN                                 FA07410
               IERB = 1                                                  FA07420
               IF (NOPRNT.GE.0) WRITE (IPR,944)                                 
               IF (ABS(ZBND(1)-ZMDL(1)).LE.0.0001) THEN                  FA07440
                  ZBND(1) = ZMDL(1)                                      FA07450
               ELSE                                                      FA07460
                  PRINT 946,ZBND(1),ZMDL(1)                              FA07470
                  IF (NOPRNT.GE.0) WRITE (IPR,946) ZBND(1),ZMDL(1)              
                  STOP ' BOUNDARIES OUTSIDE OF ATMOS'                    FA07490
               ENDIF                                                     FA07500
            ENDIF                                                        FA07510
         ENDIF                                                           FA07520
C                                                                        FA07530
C                                                                        FA08330
C        > COMPUTE THE REFRACTIVE INDEX PROFILE        <                 FA08340
C        > RFNDXM IS 1.0-INDEX                         <                 FA08350
C        > EQUATION FOR RFNDXM IS FROM LOWTRAN (REF 3) <                 FA08360
C                                                                        FA08370
         IF (NOPRNT.GE.0)                                                       
     *        WRITE(IPR,*) '   - Using LOWTRAN6 refractive index -'             
C                                                                               
         DO 170 IM = 1, IMMAX                                            FA08380
            PPH2O = DENM(1,IM)*PZERO*TM(IM)/(TZERO*ALOSMT)               FA08390
C                                                                               
C	    Approximation to refraction index (from LOWTRAN5)                         
C                                                                               
C           RFNDXM(IM) = ((77.46+0.459E-8*XVBAR**2)*PM(IM)/TM(IM)-       FA08400
C    *                   (PPH2O/1013.0)*(43.49-0.347E-8*XVBAR**2))*      FA08410
C    *                   1.0E-6                                          FA08420
C                                                                               
C	    Approximation to refraction index (from LOWTRAN6)                         
C                                                                               
            RFNDXM(IM)=((83.42+(185.08/(1.0-(XVBAR/1.14E+5)**2))+               
     *     (4.11/(1.0-(XVBAR/6.24E+4)**2)))*(PM(IM)*288.15)/                    
     *     (1013.25*TM(IM))-(43.49-(XVBAR/1.7E+4)**2)*(PPH2O/1013.25))          
     *     *1.0E-06                                                             
  170    CONTINUE                                                        FA08430
C                                                                        FA08440
C        > PRINT ATMOSPHERIC PROFILE <                                   FA08450
C                                                                        FA08460
         IF (NOPRNT.GE.0) WRITE (IPR,948) M,HMOD                                
c                                                                               
         IF (NOPRNT.EQ.0) THEN                                                  
            WRITE (IPR,950) (HMOLS(K),K=1,NMOL)                             FA08
            WRITE (IPR,952)                                                 FA08
         ENDIF                                                                  
c                                                                               
         DO 180 IM = 1, IMMAX                                            FA08510
C                                                                        FA08520
C        > DENG=DENM(1,IM)*2.989641E-17 <                                FA08530
C                                                                        FA08540
            DENAIR = ALOSMT*(PM(IM)/PZERO)*(TZERO/TM(IM))                FA08550
            densave(im) = denair                                                
            IF (NOPRNT.EQ.0)                                                    
     *           WRITE (IPR,954) IM,ZMDL(IM),PM(IM),TM(IM),RFNDXM(IM),          
     *                           DENAIR,(DENM(K,IM),K=1,NMOL)                   
                                                                                
                                                                                
 180     CONTINUE                                                        FA08580
                                                                                
         IF (NOPRNT.EQ.0) WRITE (IPR,951) (HMOLS(K),K=1,NMOL)                   
                                                                                
         DO 188 IM = 1, IMMAX                                                   
                                                                                
c     Calculate mixing ratio, using dry air                                     
                                                                                
            dry_air = densave(im)-denm(1,im)                                    
            IF (NOPRNT.EQ.0)                                                    
     *           WRITE (IPR,954) IM,ZMDL(IM),PM(IM),TM(IM),RFNDXM(IM),          
     *                DENsave(im),((DENM(K,IM)/DRY_AIR)*1.e6,K=1,NMOL)          
                                                                                
 188     continue                                                               
                                                                                
c_____________________________________________________________________________  
                                                                                
C                                                                        FA08600
C        > REDUCE SLANT PATH PARAMETERS TO STANDARD FORM <               FA08610
C                                                                        FA08620
         CALL FSCGEO (H1,H2,ANGLE,RANGE,BETA,ITYPE,LEN,HMIN,PHI,IERROR,         
     *                HOBS)                                                     
         IF (IERROR.NE.0) GO TO 310                                      FA08640
C                                                                        FA08650
C        > SET UP LBLRTM LAYER BOUNDARIES <                              FA08660
C                                                                        FA08670
         IF (IBMAX.NE.0) GO TO 200                                       FA08680
C                                                                        FA08690
C        > AUTOMATIC LAYERING SELECTED <                                 FA08700
C                                                                        FA08710
         HMAX =   MAX(H1,H2)                                             FA08720
         CALL AUTLAY (HMIN,HMAX,XVBAR,AVTRAT,TDIFF1,TDIFF2,ALTD1,ALTD2,  FA08730
     *                IERROR)                                            FA08740
         GO TO 220                                                       FA08750
  200    CONTINUE                                                        FA08760
C                                                                        FA08770
C        > USER SUPPLIED LAYERING <                                      FA08780
C                                                                        FA08790
         if (noprnt .ge. 0) WRITE (IPR,956)                                     
         IF (IBMAX_B .LT. 0) THEN                                               
            DO 205 IB = 1, IBMAX                                                
               CALL HALFWD_P(ZBND(IB),XVBAR,PBND(IB),TBND(IB),           FA08820
     *                   ALORNZ(IB),ADOPP(IB),AVOIGT(IB))                       
 205        CONTINUE                                                            
         ELSE                                                                   
            DO 210 IB = 1, IBMAX                                         FA08810
               CALL HALFWD (ZBND(IB),XVBAR,PBND(IB),TBND(IB),                   
     *                   ALORNZ(IB),ADOPP(IB),AVOIGT(IB))                FA08830
  210    CONTINUE                                                        FA08840
         ENDIF                                                                  
  220    CONTINUE                                                        FA08850
         if (noprnt .ge. 0) WRITE (IPR,958) ALZERO,AVMWT,XVBAR                  
         DO 230 IB = 1, IBMAX                                            FA08870
            ZETA = ALORNZ(IB)/(ALORNZ(IB)+ADOPP(IB))                     FA08880
            RATIO = 0.0                                                  FA08890
            DTEMP = 0.0                                                  FA08900
            IF (IB.NE.IBMAX) RATIO = AVOIGT(IB)/AVOIGT(IB+1)             FA08910
            IF (IB.NE.IBMAX) DTEMP = ABS(TBND(IB)-TBND(IB+1))            FA08920
            if (noprnt .ge. 0) WRITE (IPR,960) IB,ZBND(IB),P                    
     *                      BND(IB),TBND(IB),ALORNZ(IB),                        
     *                      ADOPP(IB),ZETA,AVOIGT(IB),RATIO,DTEMP        FA08940
  230    CONTINUE                                                        FA08950
         IF (IERROR.NE.0) STOP ' IERROR'                                 FA08960
C                                                                        FA08970
C        > CALCULATE THE REFRACTED PATH THROUGH THE ATMOSPHERE <         FA08980
C                                                                        FA08990
         CALL RFPATH (H1,H2,ANGLE,PHI,LEN,HMIN,IAMT,RANGE,BETA,BENDNG)   FA09000
C                                                                        FA09010
C        > PRINT AMOUNTS BY LAYER AND ACCUMULATE TOTALS <                FA09020
C                                                                        FA09030
         IF (NOPRNT.ge.0) WRITE (IPR,962) (HMOLS(K),K=1,NMOL)            FA09040
         I2 = IPMAX-1                                                    FA09050
         AIRTOT = 0.0                                                    FA09060
         DO 240 K = 1, NMOL                                              FA09070
            AMTTOT(K) = 0.0                                              FA09080
  240    CONTINUE                                                        FA09090
         HMID =   MIN(H1,H2)                                             FA09100
         DO 260 I = 1, I2                                                FA09110
            FAC = 1.0                                                    FA09120
            IF (LEN.EQ.1.AND.ZPTH(I+1).LE.HMID) FAC = 2.0                FA09130
            AMTAIR = RHOPSM(I)*1.0E5                                     FA09140
            AIRTOT = AIRTOT+FAC*AMTAIR                                   FA09150
            DO 250 K = 1, NMOL                                           FA09160
               AMTTOT(K) = AMTTOT(K)+FAC*AMTP(K,I)                       FA09170
  250       CONTINUE                                                     FA09180
            IF (NOPRNT.ge.0)                                             FA09190
     *           WRITE (IPR,964) I,ZPTH(I),ZPTH(I+1),AMTAIR,             FA09200
     *                           (AMTP(K,I),K=1,NMOL)                    FA09210
  260    CONTINUE                                                        FA09220
         IF (NOPRNT.ge.0)                                                FA09230
     *        WRITE (IPR,966) H1,H2,AIRTOT,(AMTTOT(K),K=1,NMOL)          FA09240
C                                                                        FA09250
C        > PRINT SUMMARY OF PATH <                                       FA09260
C                                                                        FA09270
         AIRMAS = AIRTOT/AIRMS1                                          FA09280
         if (noprnt .ge. 0) WRITE (IPR,968)                                     
     *           HMOD,H1,H2,ANGLE,RANGE,BETA,PHI,HMIN,BENDNG,LEN,AIRMAS         
         IF (ITYPE.EQ.3) ITYPE = 2                                       FA09310
         H1F = H1                                                        FA09320
         H2F = H2                                                        FA09330
         ANGLEF = ANGLE                                                  FA09340
         PHIF = PHI                                                      FA09350
         IERRF = IERROR                                                  FA09360
         LENF = LEN                                                      FA09370
          HMINF = HMIN                                                    FA0938
C                                                                        FA09390
C        > CONDENSE THE AMOUNTS INTO THE LBLRTM OUTPUT LAYERS ZOUT, <    FA09400
C        > WHICH ARE DEFINED BY THE BOUNDARIES ZBND FROM HMIN TO    <    FA09410
C        > HMAX ALSO, ZERO OUT THE AMOUNT FOR A MOLECULE IF THE     <    FA09420
C        > CUMULATIVE AMOUNT FOR THAT LAYER AND ABOVE IN LESS THAN  <    FA09430
C        > 0.1 PERCENT OF THE TOTAL                                 <    FA09440
C                                                                        FA09450
         CALL FPACK (H1,H2,HMID,LEN,IEMIT,NOZERO)                        FA09460
C                                                                        FA09470
C        > OUTPUT THE PROFILE <                                          FA09480
C                                                                        FA09490
         LMAX = IOUTMX-1                                                 FA09500
         IF (NMOL.LE.7) THEN                                             FA09510
            if (noprnt.ge.0) WRITE (IPR,970) (HMOLS(K),K=1,NMOL),COTHER         
         ELSE                                                            FA09530
            if (noprnt.ge.0) WRITE (IPR,970) (HMOLS(K),K=1,7),COTHER,           
     *                                       (HMOLS(K),K=8,NMOL)                
         ENDIF                                                           FA09560
         IF (IPUNCH.EQ.1) THEN                                           FA09570
            IFORM = 1                                                           
            WRITE (IPU,972) IFORM,LMAX,NMOL,SECNT0,(HMOD(I),I=1,2),      FA09580
     *                      H1,H2,ANGLE,LEN                              FA09590
         ENDIF                                                                  
         SUMN2 = 0.                                                      FA09600
         SUMRS = 0.                                                      FA09610
         PWTD = 0.                                                       FA09620
         TWTD = 0.                                                       FA09630
         WTOT = 0.                                                       FA09640
C                                                                               
c                                                                               
c        Read from/Write to "IFIXTYPE" file: if IFXTYP = -2, use                
C        preset ITYL values; if IFXTYP = 2, calculate and write out             
C        ITYL values.                                                           
                                                                                
         IF (IFXTYP.eq.-2) then                                                 
            open(99,file='IFIXTYPE',status='old',                               
     *           form='formatted')                                              
         elseif (IFXTYP.eq.2) then                                              
            open(99,file='IFIXTYPE',status='unknown',                           
     *           form='formatted')                                              
         endif                                                                  
                                                                                
         DO 280 L = 1, LMAX                                              FA09650
            FACTOR = 1.                                                  FA09660
            IF (IPATH(L).EQ.2) FACTOR = 2.                               FA09670
            SUMWK = 0.                                                   FA09680
            DO 270 MOL = 1, NMOL                                         FA09690
               SUMWK = SUMWK+AMOUNT(MOL,L)                               FA09700
               WMT(MOL) = WMT(MOL)+AMOUNT(MOL,L)*FACTOR                  FA09710
  270       CONTINUE                                                     FA09720
            WTOTL(L) = SUMWK+WN2L(L)                                     FA09730
            SUMN2 = SUMN2+WN2L(L)*FACTOR                                 FA09740
            SUMRS = SUMRS+RHOSUM(L)*FACTOR                               FA09750
            WTOT = WTOT+WTOTL(L)*FACTOR                                  FA09760
            PWTD = PWTD+PBAR(L)*WTOTL(L)*FACTOR                          FA09770
            TWTD = TWTD+TBAR(L)*WTOTL(L)*FACTOR                          FA09780
C                                                                               
C           > Determine ITYL(L), if desired (when setting the ratio <           
C           > from layer to layer).  Default is ITYL(L) left blank, <           
C           >                                                       <           
C           >                    CTYPE(L) = '   '                   <           
C                                                                               
            CTYPE(L) = '   '                                                    
            IF (IFXTYP.EQ.1) THEN                                               
               FRH2O  = AMOUNT(1,L)/WTOTL(L)                                    
               ALFCOR = (PBAR(L)/PZERO)*SQRT(T296/TBAR(L))                      
               ADBAR = 3.581155E-07*XVBAR*SQRT(TBAR(L)/AVMWT)                   
               CALL FIXTYP(IEMIT,FRH2O,ALFCOR,OLDDV,L,CTYPE(L))                 
               read(ctype(L),1100) ityl(l)                                      
            elseif (ifxtyp.eq.2) then                                           
               FRH2O  = AMOUNT(1,L)/WTOTL(L)                                    
               ALFCOR = (PBAR(L)/PZERO)*SQRT(T296/TBAR(L))                      
               ADBAR = 3.581155E-07*XVBAR*SQRT(TBAR(L)/AVMWT)                   
               CALL FIXTYP(IEMIT,FRH2O,ALFCOR,OLDDV,L,CTYPE(L))                 
               read(ctype(l),1100) ityl(l)                                      
               write(99,1100) ityl(l)                                           
            elseif (ifxtyp.eq.-2) then                                          
               read(99,1100) ityl(l)                                            
               write(ctype(l),1100) ityl(l)                                     
            ENDIF                                                               
C                                                                               
C                                                                               
C           > Write atmosphere to TAPE6 in column density <                     
C                                                                               
            if (noprnt .ge. 0) then                                             
               IF (NMOL.LE.7) THEN                                              
                  WRITE (IPR,974) L,ZOUT(L),ZOUT(L+1),CTYPE(L),IPATH(L),        
     *                            PBAR(L),TBAR(L),RHOSUM(L),                    
     *                            (AMOUNT(K,L),K=1,NMOL),WN2L(L)                
               ELSE                                                             
                  WRITE (IPR,976) L,ZOUT(L),ZOUT(L+1),CTYPE(L),IPATH(L),        
     *                            PBAR(L),TBAR(L),RHOSUM(L),                    
     *                            (AMOUNT(K,L),K=1,7),WN2L(L),                  
     *                            (AMOUNT(K,L),K=8,NMOL)                        
               ENDIF                                                            
            endif                                                               
C                                                                               
C           > Write atmosphere to TAPE7 <                                       
C                                                                               
            IF (IPUNCH.EQ.1) THEN                                        FA09890
               LTST = L                                                  FA09900
               IF (L.EQ.1) LTST = 0                                      FA09910
               PTST =  LOG10(PZ(LTST))                                   FA09920
               NPTST = PTST+2                                            FA09930
               IF (PTST.LT.0.0) NPTST = 1                                FA09940
               CFORM1(38:41) = PZFORM(NPTST)                             FA09950
               CFORM2(38:41) = PZFORM(NPTST)                             FA09960
               NPTST = 1                                                 FA09970
               IF (PBAR(L).GE.0.1) NPTST = 2                             FA09980
               CFORM1(2:8) = PAFORM(NPTST)                               FA09990
               CFORM2(2:8) = PAFORM(NPTST)                               FA10000
               IF (L.EQ.1) THEN                                          FA10010
                  WRITE (IPU,CFORM1) PBAR(L),TBAR(L),CTYPE(L),IPATH(L),  FA10020
     *                               ALTZ(L-1),PZ(L-1),TZ(L-1),          FA10030
     *                               ALTZ(L),  PZ(L),  TZ(L)             FA10040
               ELSE                                                      FA10050
                  WRITE (IPU,CFORM2) PBAR(L),TBAR(L),CTYPE(L),IPATH(L),  FA10060
     *                               ALTZ(L),  PZ(L),  TZ(L)             FA10070
               ENDIF                                                     FA10080
C                                                                               
C           -------------------------------------                               
C           > Write molecular information in    <                               
C           >  - mixing ratio if MUNITS is 1    <                               
C           >  - column density if MUNITS is 0  <                               
C           -------------------------------------                               
C                                                                               
               IF (MUNITS.EQ.1) THEN                                            
                  DRAIR =  WN2L(L)                                              
                  DO 275 M = 2,NMOL                                             
                     DRAIR = DRAIR + AMOUNT(M,L)                                
 275              CONTINUE                                                      
C                                                                               
C                 > If DRAIR is zero, then write out AMOUNT only    <           
C                 > (since AMOUNT zero => mixing ratio zero)        <           
C                                                                               
                  IF (DRAIR.EQ.0) THEN                                          
                     WRITE (IPU,978) (AMOUNT(K,L),K=1,7),WN2L(L)                
                     IF (NMOL.GT.7) WRITE (IPU,978)                             
     *                                    (AMOUNT(K,L),K=8,NMOL)                
                  ELSE                                                          
                     WRITE (IPU,978) (AMOUNT(K,L)/DRAIR,K=1,7),WN2L(L)          
                     IF (NMOL.GT.7) WRITE (IPU,978)                             
     *                                    (AMOUNT(K,L)/DRAIR,K=8,NMOL)          
                  ENDIF                                                         
               ELSE                                                             
C                                                                               
C                 Test to make sure there are no fractional molecular           
C                 amounts written out (will cause PATH to assume                
C                 mixing ratio)                                                 
C                                                                               
                  DO 277 K=1,NMOL                                               
                     IF (AMOUNT(K,L).LT.1.) THEN                                
                        WRITE(IPR,1000) K,L                                     
                        AMOUNT(K,L) = 0.0                                       
                     ENDIF                                                      
 277              CONTINUE                                                      
C                                                                               
                  WRITE (IPU,978) (AMOUNT(K,L),K=1,7),WN2L(L)            FA10090
                  IF (NMOL.GT.7) WRITE (IPU,978) (AMOUNT(K,L),K=8,NMOL)  FA10100
               ENDIF                                                            
            ENDIF                                                        FA10110
  280    CONTINUE                                                        FA10120
                                                                                
C        Close "IFIXTYPE" file, if used                                         
         IF (abs(IFXTYP).eq.2) then                                             
            rewind(99)                                                          
            close(99)                                                           
         endif                                                                  
C                                                                               
C        > Write atmosphere to TAPE6 in mixing ratio <                          
C                                                                               
         if (noprnt .ge. 0) then                                                
            IF (NMOL.LE.7) THEN                                                 
               WRITE (IPR,973) (HMOLS(K),K=1,NMOL),COTHER                       
            ELSE                                                                
               WRITE (IPR,973) (HMOLS(K),K=1,7),COTHER,                         
     *              (HMOLS(K),K=8,NMOL)                                         
            ENDIF                                                               
         endif                                                                  
c                                                                               
         DO 285 L = 1, LMAX                                                     
            DRAIR = WN2L(L)                                                     
            DO 283 M = 2,NMOL                                                   
               DRAIR = DRAIR + AMOUNT(M,L)                                      
 283        CONTINUE                                                            
            COLDRY(L) = DRAIR
            NMOLEC = NMOL
C                                                                               
C           > If DRAIR is zero, then write out AMOUNT only    <                 
C           > (since AMOUNT zero => mixing ratio zero)        <                 
C                                                                               
            IF (DRAIR.EQ.0 .and. noprnt.ge.0) THEN                              
               IF (NMOL.LE.7) THEN                                              
                  WRITE (IPR,974) L,ZOUT(L),ZOUT(L+1),CTYPE(L),IPATH(L),        
     *                            PBAR(L),TBAR(L),RHOSUM(L),                    
     *                            (AMOUNT(K,L),K=1,NMOL),WN2L(L)                
               ELSE                                                             
                  WRITE (IPR,976) L,ZOUT(L),ZOUT(L+1),CTYPE(L),IPATH(L),        
     *                            PBAR(L),TBAR(L),RHOSUM(L),                    
     *                            (AMOUNT(K,L),K=1,7),WN2L(L),                  
     *                            (AMOUNT(K,L),K=8,NMOL)                        
               ENDIF                                                            
            ELSE if (noprnt.ge.0) then                                          
               IF (NMOL.LE.7) THEN                                              
                  WRITE (IPR,974) L,ZOUT(L),ZOUT(L+1),CTYPE(L),IPATH(L),        
     *                            PBAR(L),TBAR(L),RHOSUM(L),                    
     *                            (AMOUNT(K,L)/DRAIR,K=1,NMOL),WN2L(L)          
               ELSE                                                             
                  WRITE (IPR,976) L,ZOUT(L),ZOUT(L+1),CTYPE(L),IPATH(L),        
     *                            PBAR(L),TBAR(L),RHOSUM(L),                    
     *                            (AMOUNT(K,L)/DRAIR,K=1,7),WN2L(L),            
     *                            (AMOUNT(K,L)/DRAIR,K=8,NMOL)                  
               ENDIF                                                            
            ENDIF                                                               
285     CONTINUE                                                                
C                                                                               
         PWTD = PWTD/WTOT                                                FA10130
         TWTD = TWTD/WTOT                                                FA10140
         L = LMAX                                                        FA10150
         if (noprnt .ge. 0) then                                                
            IF (NMOL.LE.7) THEN                                                 
               WRITE (IPR,980) (HMOLS(K),K=1,NMOL),COTHER                   FA10
               WRITE (IPR,982) L,ZOUT(1),ZOUT(L+1),PWTD,TWTD,SUMRS,         FA10
     *                         (WMT(K),K=1,NMOL),SUMN2                      FA10
            ELSE                                                            FA10
               WRITE (IPR,980) (HMOLS(K),K=1,7),COTHER,                     FA10
     *                         (HMOLS(K),K=8,NMOL)                          FA10
               WRITE (IPR,984) L,ZOUT(1),ZOUT(L+1),PWTD,TWTD,SUMRS,         FA10
     *                         (WMT(K),K=1,7),SUMN2,(WMT(K),K=8,NMOL)       FA10
            ENDIF                                                           FA10
         endif                                                                  
C                                                                        FA10260
         NLAYRS = LMAX                                                   FA10270
         HT1 = HT1SLT                                                           
         HT2 = HT2SLT                                                           
C                                                                        FA10300
      ENDIF                                                              FA10310
C                                                                        FA10320
c-----------------------------------------------------------                    
c compute layer-to-level conversion for analytical jacobians                    
c pbar,tbar                                                                     
c only go into this if imoldq was set in lblrtm                                 
c                                                                               
c note that the dqdl and dqdu arrays are indexed by mol-id                      
c number with the "0" index eserved for temperature                             
c                                                                               
      if (imoldq.eq.-99) then                                                   
c          write(*,*) 'lay2lev in lblatm: ',ibmax,nmol                          
          ilevdq=ibmax-1                                                        
          imoldq=nmol                                                           
          do 500 i=1,ilevdq                                                     
                                                                                
              rhoU=pbnd(i+1)/(tbnd(i+1)*1.3806503E-19)                          
              rhoL=pbnd(i)/(tbnd(i)*1.3806503E-19)                              
              alpha=rhoU/rhoL                                                   
              alphaT=-(tbnd(i+1)-tbnd(i))/alog(alpha)                           
                                                                                
c molecules                                                                     
              do 501 k=1,nmol                                                   
                                                                                
                  if (denm(k,i).ne.0.0) then                                    
                                                                                
                      ratU=denm(k,i+1)/rhoU                                     
                      ratL=denm(k,i)/rhoL                                       
                                                                                
                      dqdL(i,k)=(ratL/(ratL-alpha*ratU))                        
     &                    +1.0/alog(alpha*ratU/ratL)                            
                                                                                
                      dqdU(i,k)=((-alpha*ratU)/(ratL-alpha*ratU))               
     &                    -1.0/alog(alpha*ratU/ratL)                            
                                                                                
c                      write(*,*) i,k,((dqdL(i,k)*ratL)                         
c     &                    +(dqdU(i,k)*ratU)),                                  
c     &                    ratL,ratU                                            
c                      write(*,*) '      ',denm(k,i+1),rhoU                     
c                      write(*,*) '      ',denm(k,i),rhoL                       
                                                                                
                  else                                                          
                      dqdL(i,k)=0.0                                             
                      dqdU(i,k)=0.0                                             
                                                                                
                                                                                
c check to be sure molecular amount non-zero for molecular jacobian             
                      if (k.eq.nspcrt) then                                     
                          write(*,*) ' --- FATAL ERROR ---'                     
                          write(*,*) 'molecular amount for species ',k          
                          write(*,*) '     must be non-zero '                   
                          write(*,*) 'for analytic jacobian #',nspcrt           
                          write(*,*) ' -------------------'                     
                          STOP                                                  
                      endif                                                     
                                                                                
                  endif                                                         
                                                                                
  501         continue                                                          
                                                                                
c temperature                                                                   
              dqdL(i,0)=((tbar(i)-alphaT)/tbnd(i))                              
     &            *(rhoL/(rhoL-rhoU))                                           
     &            +(1.0-alphaT/tbnd(i))/alog(alpha)                             
                                                                                
              dqdU(i,0)=((tbar(i)-alphaT)/tbnd(i+1))                            
     &                  *(-rhoU/(rhoL-rhoU))                                    
     &            -(1.0-alphaT/tbnd(i+1))/alog(alpha)                           
                                                                                
c              write(*,*) 'T: ',dqdl(i,0),dqdu(i,0)                             
                                                                                
  500     continue                                                              
      endif                                                                     
c-----------------------------------------------------------                    
                                                                                
      RETURN                                                             FA10330
C                                                                        FA10340
C     ERROR MESSAGES                                                     FA10350
C                                                                        FA10360
  290 WRITE (IPR,986) MODEL,ITYPE,NMOL,IBMAX                             FA10370
C                                                                        FA10380
      STOP ' CARD 3.1'                                                   FA10390
C                                                                        FA10400
C                                                                        FA10400
  300 WRITE (IPR,988) (ZBND(I),I=1,IBMAX)                                FA10410
      PRINT 988,(ZBND(I),I=1,IBMAX)                                      FA10420
C                                                                        FA10430
      STOP ' ZBND'                                                       FA10440
                                                                                
  301 PRINT 988,(ZTMP(I),I=2,IBMAX)                                             
      STOP ' USER INPUT LEVELS TOO CLOSE - IBMAX'                               
C                                                                               
  305 WRITE (IPR,989) (PBND(I),I=1,IBMAX)                                       
      PRINT 989,(PBND(I),I=1,IBMAX)                                             
C                                                                        FA10430
      STOP ' PBND'                                                       FA10440
C                                                                        FA10450
  310 WRITE (IPR,990)                                                    FA10460
C                                                                        FA10470
      STOP ' ERROR - FSCGEO'                                             FA10480
C                                                                        FA10490
  320 WRITE (IPR,992) AVTRAT,TDIFF1,TDIFF2                               FA10500
C                                                                        FA10510
      STOP ' AVTRAT,TDIFF'                                               FA10520
C                                                                        FA10530
  900 FORMAT (7I5,I2,1X,I2,4F10.3,F10.3)                                 FA10540
  902 FORMAT (' CONTROL CARD 3.1: MODEL AND OPTIONS ')                   FA10550
  904 FORMAT (/,10X,'MODEL   = ',I5,/,10X,'ITYPE   = ',I5,/,10X,         FA10560
     *        'IBMAX   = ',I5,/,10X,'NOZERO  = ',I5,/,10X,'NOPRNT  = ',  FA10570
     *        I5,/,10X,'NMOL    = ',I5,/,10X,'IPUNCH  = ',I5,/,10X,      FA10580
     *        'IFXTYP  = ',I5,/,10X,'MUNITS  = ',I5,/,10X,'RE      = ',  FA10590
     *        F10.3,' KM',/,10X,'HSPACE  = ',F10.3,' KM',/,10X,          FA10600
     *        'VBAR    = ',F10.3,' CM-1',/,10X,'CO2MX   = ',                    
     *        F10.3,' PPM',/,10X,'REF_LAT = ',F10.3, ' DEG')             FA10610
  905 format('$',i5, 10a8)                                                      
  906 FORMAT (///,' CONTROL CARD 3.1 PARAMETERS WITH DEFAULTS:')         FA10620
  908 FORMAT (///,' HORIZONTAL PATH SELECTED')                           FA10630
  910 FORMAT (F10.3,10X,10X,F10.3)                                       FA10640
  912 FORMAT (///,' CONTROL CARD 3.2:',//,10X,'Z     = ',F10.3,' KM',/,  FA10650
     *       10X,'RANGE = ',F10.3,' KM')                                 FA10660
  914 FORMAT (///' PRESSURE, TEMPERATURE, AND DENSITIES INTERPOLATED',   FA10670
     *        ' FROM THE FOLLOWING ATMOSPHERIC MODEL: ',//,10X,3A8,//,   FA10680
     *        10X, 'Z     = ',F10.3,' KM',/,10X,'P     = ',F10.3,' MB',  FA10690
     *        /,10X,'T     = ',F10.3,' K',//,10X,'DENSITIES :',T26,      FA10700
     *        'AIR',(T30,8A10))                                          FA10710
  916 FORMAT (T63,'(MOL CM-3)',//,T20,1PE10.3,(T30,8E10.3))              FA10720
  918 FORMAT ('0SINGLE LAYER INPUT TO LBLRTM',//,10X,'MODEL = ',3A8,/,   FA10730
     *        10X,'Z     = ',F10.3,' KM',/,10X,'P     = ',F10.3,' MB',   FA10740
     *        /,10X,'T     = ',F10.3,' K',/,10X,'RANGE = ',F10.3,' KM',  FA10750
     *        //,10X,'AMOUNTS (MOL CM-2):',T36,'AIR',(T32,8A10))         FA10760
  920 FORMAT (//,T30,1PE10.2,(T30,8E10.2))                               FA10770
  922 FORMAT (A4)                                                        FA10780
  924 FORMAT (1X,I1,I3,I5,F10.6,3A8,' * ',F7.3,' KM PATH AT ',F7.3,             
     *        ' KM ALT')                                                 FA10790
  926 FORMAT (E15.7,F10.4,10X,I5,1X,F7.3,15X,F7.3,/,(1P8E15.7))          FA10800
  928 FORMAT (//,' MULTIPLE SCATTERING TURNED OFF, HMIN = ',F10.6,       FA10810
     *        ' > HMAXMS = ',F10.6,/)                                    FA10820
  930 FORMAT (///,' SLANT PATH SELECTED, ITYPE = ',I5)                   FA10830
  932 FORMAT (5F10.4,I5,5X,F10.4)                                        FA10840
  933 FORMAT (///' CONTROL CARD 3.2:  SLANT PATH PARAMETERS',//,10X,     FA10850
     *       'H1      = ',F12.6,' MBAR',/,10X,'H2      = ',F12.6,' MBAR',FA10860
     *        /,10X,'ANGLE   = ',F12.6,' DEG',/,10X,'RANGE   = ',F12.6,  FA10870
     *        ' KM',/,10X,'BETA    = ',F12.6,' DEG',/,10X,'LEN     = ',  FA10880
     *        I10)                                                       FA10890
  934 FORMAT (///' CONTROL CARD 3.2:  SLANT PATH PARAMETERS',//,10X,     FA10850
     *        'H1      = ',F12.6,' KM',/,10X,'H2      = ',F12.6,' KM',   FA10860
     *        /,10X,'ANGLE   = ',F12.6,' DEG',/,10X,'RANGE   = ',F12.6,  FA10870
     *        ' KM',/,10X,'BETA    = ',F12.6,' DEG',/,10X,'LEN     = ',  FA10880
     *        I10)                                                       FA10890
  936 FORMAT (5F10.3)                                                    FA10900
  938 FORMAT (///,' AUTOLAYERING SELECTED',//,10X,'AVTRAT    = ',F8.2,   FA10910
     *        /,10X,'TDIFF1    = ',F8.2,/,10X,'TDIFF2    = ',F8.2,/,     FA10920
     *        10X,'ALTD1     = ',F8.2,/,10X,'ALTD2     = ',F8.2)         FA10930
  940 FORMAT (8F10.3)                                                    FA10940
  942 FORMAT (///,' USER DEFINED BOUNDARIES FOR LBLRTM LAYERS',/,10X,    FA10950
     *        'I',4X,'Z (KM)',//,(10X,I4,F10.4))                         FA10960
  943  FORMAT (///,' USER DEFINED BOUNDARIES FOR LBLRTM LAYERS',/,10X,          
     *        'I',4X,'P (MB)',//,(10X,I4,F10.4))                                
  944 FORMAT (' ERROR IN USER INPUT BOUNDARIES ')                        FA10970
  946 FORMAT (' BOUNDARIES ARE OUTSIDE THE RANGE OF THE ATMOSPHERE',/,   FA10980
     *        ' BOUNDARY = ',F10.2,' ATMOSPHERE =',F10.2,/,              FA10990
     *        ' RESET BOUNDARY GT THAN ATMOSPHERE')                      FA11000
  948 FORMAT ('1ATMOSPHERIC PROFILE SELECTED IS: M = ',I3,5X,3A8)        FA11010
  950 FORMAT (/,T4,'I',T13,'Z',T22,'P',T34,'T',T42,'REFRACT',T73,        FA11020
     *        'DENSITY  (MOLS CM-3)',/,T42,'INDEX-1',/,T12,'(KM)',T21,   FA11030
     *        '(MB)',T33,'(K)',T42,'*1.0E6',T59,'AIR',(T64,8(6X,A9)))    FA11040
 951  FORMAT (/,T4,'I',T13,'Z',T22,'P',T34,'T',T42,'REFRACT',T55,               
     *        'DENSITY',T70,'MIXING RATIO (BASED UPON DRY AIR) (ppmv)',/,       
     *        T42,'INDEX-1',T52,                                                
     *        '(MOL CM-3)'/,T12,                                                
     *        '(KM)',T21,                                                       
     *        '(MB)',T33,'(K)',T42,'*1.0E6',T57,'AIR',(T64,8(6X,A9)))    FA11040
  952 FORMAT (/)                                                         FA11050
  954 FORMAT (I4,F11.5,F11.5,F11.5,6P,F11.5,1P,E15.7,(T64,1P,8E15.7))           
  956 FORMAT (///,' HALFWIDTH INFORMATION ON THE USER SUPPLIED ',        FA11070
     *        'LBLRTM BOUNDARIES',/,' THE FOLLOWING VALUES ARE ',        FA11080
     *        'ASSUMED:')                                                FA11090
  958 FORMAT (10X,'ALZERO    = ',F9.3,' CM-1 = AVERAGE LORENTZ WIDTH ',  FA11100
     *        'AT STP',/,10X,'AVMWT     = ',F8.2,                        FA11110
     *        '       = AVERAGE  MOLECULAR WEIGHT',/,10X,                FA11120
     *        'VBAR      = ',F8.2,'  CM-1 = AVERAGE WAVENUMBER',///,     FA11130
     *        T5,'I',T12,'Z',T22,'P',T32,'T',T39,'LORENTZ',T49,          FA11140
     *        'DOPPLER',T61,'ZETA',T70,'VOIGT',T80,'VOIGT',T90,          FA11150
     *        'TEMP',/,T11,'(KM)',T21,'(MB)',T31,'(K)',T40,'(CM-1)',     FA11160
     *        T50,'(CM-1)',T70,'(CM-1)',T80,'RATIO',T90,'DIFF (K)',/)    FA11170
  960 FORMAT (I5,F10.3,F12.5,F9.2,F9.5,F10.5,F10.3,F10.5,F10.2,F10.1)    FA11180
  962 FORMAT ('1INTEGRATED ABSORBER AMOUNTS BY LAYER',///,T5,            FA11190
     *        'I  LAYER BOUNDARIES',T55,'INTEGRATED AMOUNTS ',           FA11200
     *        '(MOL CM-2)',/,T11,'FROM',T22,'TO',T29,'AIR',T36,          FA11210
     *        8(1X,A8,1X),/,T11,'(KM)',T21,'(KM)',(T37,8A10))            FA11220
  964 FORMAT (I5,2F10.3,1P,E10.3,(T36,1P,8E10.3))                               
  966 FORMAT ('0TOTAL',F9.3,F10.3,1PE10.3,(T35,1P8E10.3))                       
  968 FORMAT ('1 SUMMARY OF THE GEOMETRY CALCULATION',//,10X,            FA11250
     *        'MODEL   = ',4X,3A8,/10X,'H1      = ',F12.6,' KM',/,10X,   FA11260
     *        'H2      = ',F12.6,' KM',/,10X,'ANGLE   = ',F12.6,' DEG',  FA11270
     *        /,10X,'RANGE   = ',F12.6,' KM',/,10X,'BETA    = ',F12.6,   FA11280
     *        ' DEG',/,10X,'PHI     = ',F12.6,' DEG',/,10X,              FA11290
     *        'HMIN    = ',F12.6,' KM',/,10X,'BENDING = ',F12.6,' DEG',  FA11300
     *        /,10X,'LEN     = ',I10,/,10X,'AIRMAS  = ',G12.6,           FA11310
     *        'RELATIVE TO A VERTICAL PATH , GROUND TO SPACE')           FA11320
  970 FORMAT ('0FINAL SET OF LAYERS FOR INPUT TO LBLRTM',/,              FA11330
     *        ' A LAYER AMOUNT MAY BE SET TO ZERO IF THE CUMULATIVE ',   FA11340
     *        'AMOUNT FOR THAT LAYER AND ABOVE IS LESS THAN 0.1 ',       FA11350
     *        'PERCENT',/,' OF THE TOTAL AMOUNT. THIS IS DONE ONLY ',    FA11360
     *        'FOR THE FOLLOWING CASES',/,5X,'1.  IEMIT = 0 ',           FA11370
     *        '(TRANSMITTANCE)',/,5X,'2.  IEMIT = 1 (RADIANCE) AND ',    FA11380
     *        'IPATH = 3 (PATH LOOKING UP)',/,' O2 IS NOT INCLUDED',/,   FA11390
     *        ' IF THE AMOUNTS FOR ALL THE MOLECULES BUT O2 ARE ',       FA11400
     *        'ZEROED, THE REMAINING LAYERS ARE ELIMINATED',///,T13,     FA11410
     *        'LAYER',T23,'I',T25,'I',/,T4,'L',T10,'BOUNDARIES',T23,     FA11420
     *        'T',T25,'P',T31,'PBAR',T40,'TBAR',T70,                            
     *        'INTEGRATED AMOUNTS (MOLS CM-2)',/,T9,'FROM',T18,          FA11430
     *        'TO',T23,'Y',T25,'T',/,T9,'(KM)',T17,'(KM)',T23,'P',T25,   FA11440
     *        'H',T31,'(MB)',T41,'(K)',T53,'AIR',(T59,8(6X,A9)))         FA11450
  972 FORMAT (1X,I1,I3,I5,F10.6,2A8,' H1=',F8.2,' H2=',F8.2,             FA11460
     *        ' ANG=',F8.3,' LEN=',I2)                                   FA11470
  973 FORMAT  (///,'1',3X,'------------------------------------',/,             
     *        T13,'LAYER',T23,'I',T25,'I',/,T4,'L',T10,'BOUNDARIES',            
     *        T23,'T',T25,'P',T31,'PBAR',T40,'TBAR',                            
     *        T68,'MOLECULAR MIXING RATIOS BY LAYER',/,T9,'FROM',               
     *        T18,'TO',T23,'Y',T25,'T',/,T9,'(KM)',T17,'(KM)',T23,'P',          
     *        T25,'H',T31,'(MB)',T41,'(K)',T53,'AIR',(T59,8(6X,A9)))            
  974 FORMAT ('0',I3,2F8.3,A3,I2,F11.5,F8.2,1X,1P9E15.7)                 FA11480
  976 FORMAT ('0',I3,2F8.3,A3,I2,F11.5,F8.2,1X,1P9E15.7,/,                      
     *            (60X,1P8E15.7))                                        FA11490
  978 FORMAT (1P8E15.7)                                                  FA11500
  980 FORMAT ('0',/,'0',T4,'L  PATH BOUNDARIES',T28,'PBAR',T37,'TBAR',   FA11510
     *        T65,'ACCUMULATED MOLECULAR AMOUNTS FOR TOTAL PATH',/,T9,   FA11520
     *        'FROM',T18,'TO',/,T9,'(KM)',T17,'(KM)',T28,'(MB)',T38,     FA11530
     *        '(K)',T47,'AIR',(T54,8(1X,A9)))                            FA11540
  982 FORMAT ('0',I3,2F8.3,2X,F11.5,F8.2,1X,1P9E10.3)                    FA11550
  984 FORMAT ('0',I3,2F8.3,2X,F11.5,F8.2,1X,1P9E10.3,/,(52X,1P8E10.3))   FA11560
  986 FORMAT (///,' ERROR IN INPUT, CONTROL CARD 3.1: ONE OF THE ',      FA11570
     *        'PARAMETERS MODEL, ITYPE, NMOL IS OUT OF RANGE',//,10X,    FA11580
     *        'MODEL   = ',I5,/,10X,'ITYPE   = ',I5,/,10X,'NMOL    = ',  FA11590
     *        I5,10X,' IBMAX =',I5)                                      FA11600
  988 FORMAT (///,' ERROR: BOUNDARY ALTITUDES FOR LBLRTM LAYERS ',       FA11610
     *        'ARE NEGATIVE OR NOT IN ASCENDING ORDER',//,5X,' ZBND ',   FA11620
     *        /,(10F10.4))                                               FA11630
  989 FORMAT (///,' ERROR: BOUNDARY PRESSURES FOR LBLRTM LAYERS ',              
     *        'ARE POSITIVE OR NOT IN DESCENDING ORDER',//,5X,' PBND ',         
     *        /,(10F10.4))                                                      
  990 FORMAT ('0ERROR FLAG RETURNED FROM FSCGEO:  AN ERROR OCCURED ',    FA11640
     *        'IN PROCESSING THE SLANT PATH PARAMETERS',/,'0PROGRAM ',   FA11650
     *        'STOP')                                                    FA11660
  992 FORMAT (///,' ERROR: EITHER AVTRAT.LE.1.0 OR TDIFF.LE.0',/,        FA11670
     *        '0PROGRAM STOP  -  AVTRAT = ',E12.6,' TDIFF1 = ',F10.4,    FA11680
     *        ' TDIFF2 = ',F10.4)                                        FA11690
 1000 FORMAT ('*** WARNING: Zeroing molecule #',i2.2,' amount ',                
     *        'in layer #',i3.3)                                                
 1100 format (i3)                                                               
 1101 format (a3)                                                               
C                                                                        FA11700
      END                                                                FA11710
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      BLOCK DATA ATMCON                                                  FA11720
C                                                                        FA11730
C     *****************************************************************  FA11740
C     THIS SUBROUTINE INITIALIZES THE CONSTANTS USED IN THE              FA11750
C     PROGRAM. CONSTANTS RELATING TO THE ATMOSPHERIC PROFILES ARE STORE  FA11760
C     IN BLOCK DATA MLATMB.                                              FA11770
C     *****************************************************************  FA11780
C                                                                        FA11790
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA11800
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA11810
C                                                                        FA11820
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA11840
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA11850
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
C                                                                        FA11880
      CHARACTER*8      HMOLS                                            &FA11890
C                                                                        FA11900
      COMMON /HMOLS/ HMOLS(MXMOL),JUNIT(MXMOL),WMOL(MXMOL),JUNITP,       FA11910
     *               JUNITT                                              FA11920
      COMMON /HMOLC/ HMOLC(MXMOL)                                        FA11930
      COMMON /FIXITYL/ IFXTYP                                                   
      CHARACTER*8 HMOLC                                                  FA11940
C     IFXTYP is the flag for fixing the value of ITYL                           
      DATA IFXTYP /0/                                                           
C                                                                        FA11950
C     IBDIM IS THE MAXIMUM NUMBER OF LAYERS FOR OUTPUT TO LBLRTM         FA11960
C     IOUTDM IS THE MAXIMUN NUMBER OF OUTPUT LAYERS                      FA11970
C     IMDIM IS THE MAX NUMBER OF LEVELS IN THE ATMOSPHERIC PROFILE       FA11980
C         STORED IN ZMDL (INPUT)                                         FA11990
C     IPDIM IS THE MAXIMUM NUMBER OF LEVELS IN THE PROFILE ZPTH OBTAINE  FA12000
C         BY MERGING ZMDL AND ZOUT                                       FA12010
C     KDIM IS THE MAXIMUM NUMBER OF MOLECULES, KMXNOM IS THE DEFAULT     FA12020
C                                                                        FA12030
      DATA KMXNOM / 7 /                                                  FA12040
C                                                                        FA12050
C     DELTAS IS THE NOMINAL SLANT PATH INCREMENT IN KM.                  FA12060
C                                                                        FA12070
      DATA DELTAS / 5.0 /                                                FA12080
      DATA PZERO / 1013.25 /,TZERO / 273.15 /                            FA12090
C                                                                        FA12130
C     ALZERO IS THE MEAN LORENTZ HALFWIDTH AT PZERO AND 296.0 K.         FA12140
C     AVMWT IS THE MEAN MOLECULAR WEIGHT USED TO AUTOMATICALLY           FA12150
C     GENERATE THE LBLRTM BOUNDARIES IN AUTLAY                           FA12160
C                                                                        FA12170
      DATA ALZERO / 0.04 /,AVMWT / 36.0 /                                FA12180
C                                                                        FA12190
C     ORDER OF MOLECULES H2O(1), CO2(2), O3(3), N2O(4), CO(5), CH4(6),   FA12200
C         O2(7), NO(8), SO2(9), NO2(10), NH3(11), HNO3(12), OH(13),      FA12210
C         HF(14 ), HCL(15), HBR(16), HI(17), CLO(18), OCS(19), H2CO(20)  FA12220
C         HOCL(21), N2(22), HCN(23), CH3CL(24), H2O2(25), C2H2(26),      FA12230
C         C2H6(27), PH3(28), COF2(29), SF6(30), H2S(31), HCOOH(32)       FA12240
C                                                                        FA12250
      DATA HMOLC / '  H2O   ' , '  CO2   ' , '   O3   ' , '  N2O   ' ,   FA12260
     *             '   CO   ' , '  CH4   ' , '   O2   ' , '   NO   ' ,   FA12270
     *             '  SO2   ' , '  NO2   ' , '  NH3   ' , ' HNO3   ' ,   FA12280
     *             '   OH   ' , '   HF   ' , '  HCL   ' , '  HBR   ' ,   FA12290
     *             '   HI   ' , '  CLO   ' , '  OCS   ' , ' H2CO   ' ,   FA12300
     *             ' HOCL   ' , '   N2   ' , '  HCN   ' , ' CH3CL  ' ,   FA12310
     *             ' H2O2   ' , ' C2H2   ' , ' C2H6   ' , '  PH3   ' ,   FA12320
     *             ' COF2   ' , '  SF6   ' , '  H2S   ' , ' HCOOH  ' ,   FA12330
     *             '  HO2   ' , '   O+   ' , ' ClONO2 ' , '   NO+  ' ,          
     *             '  HOBr  ' , ' C2H4   ' /                                    
C                                                                        FA12350
C     MOLECULAR WEIGHTS                                                  FA12360
C                                                                        FA12370
      DATA AIRMWT / 28.964 / ,                                           FA12380
     *     AMWT /   18.015 ,  44.010 , 47.998 , 44.01 ,                  FA12390
     *              28.011 ,  16.043 , 31.999 , 30.01 ,                  FA12400
     *              64.06  ,  46.01  , 17.03  , 63.01 ,                  FA12410
     *              17.00  ,  20.01  , 36.46  , 80.92 ,                  FA12420
     *             127.91  ,  51.45  , 60.08  , 30.03 ,                  FA12430
     *              52.46  ,  28.014 , 27.03  , 50.49 ,                  FA12440
     *              34.01  ,  26.03  , 30.07  , 34.00 ,                  FA12450
     *              66.01  , 146.05  , 34.08  , 46.03 ,                  FA12460
c     approx;                                                                   
     *              33.00  ,  15.99  , 98.    , 30.00 ,                         
     *              97.    ,  44.5   /                                          
      END                                                                FA12480
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      BLOCK DATA MLATMB                                                  FA12490
C                                                                        FA12500
C     *****************************************************************  FA12510
C     THIS SUBROUTINE INITIALIZES THE 6 BUILT-IN ATMOSPHERIC PROFILES    FA12520
C     (FROM 'OPTICAL PROPERTIES OF THE ATMOSPHERE, THIRD EDITION'        FA12530
C     AFCRL-72-0497 (AD 753 075), 'U.S. STANDARD ATMOSPHERE 1976' AND    FA12540
C     'SUPPLEMENTS 1966'), PLUS COLLECTED CONSTITUENT PROFILES (REF)     FA12550
C     AND SETS OTHER CONSTANTS RELATED TO THE ATMOSPHERIC PROFILES       FA12560
C     *****************************************************************  FA12570
C                                                                        FA12580
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA12590
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA12600
      PARAMETER (MXZ50=MXZMD-50)                                         FA12610
C                                                                        FA12620
      COMMON /MLATM/ ALT(MXZMD),                                         FA12630
     *     P1(MXZMD),P2(MXZMD),P3(MXZMD),P4(MXZMD),P5(MXZMD),P6(MXZMD),  FA12640
     *     T1(MXZMD),T2(MXZMD),T3(MXZMD),T4(MXZMD),T5(MXZMD),T6(MXZMD),  FA12650
     *     AMOL11(MXZMD),AMOL12(MXZMD),AMOL13(MXZMD),AMOL14(MXZMD),      FA12660
     *     AMOL15(MXZMD),AMOL16(MXZMD),AMOL17(MXZMD),AMOL18(MXZMD),      FA12670
     *     AMOL21(MXZMD),AMOL22(MXZMD),AMOL23(MXZMD),AMOL24(MXZMD),      FA12680
     *     AMOL25(MXZMD),AMOL26(MXZMD),AMOL27(MXZMD),AMOL28(MXZMD),      FA12690
     *     AMOL31(MXZMD),AMOL32(MXZMD),AMOL33(MXZMD),AMOL34(MXZMD),      FA12700
     *     AMOL35(MXZMD),AMOL36(MXZMD),AMOL37(MXZMD),AMOL38(MXZMD),      FA12710
     *     AMOL41(MXZMD),AMOL42(MXZMD),AMOL43(MXZMD),AMOL44(MXZMD),      FA12720
     *     AMOL45(MXZMD),AMOL46(MXZMD),AMOL47(MXZMD),AMOL48(MXZMD),      FA12730
     *     AMOL51(MXZMD),AMOL52(MXZMD),AMOL53(MXZMD),AMOL54(MXZMD),      FA12740
     *     AMOL55(MXZMD),AMOL56(MXZMD),AMOL57(MXZMD),AMOL58(MXZMD),      FA12750
     *     AMOL61(MXZMD),AMOL62(MXZMD),AMOL63(MXZMD),AMOL64(MXZMD),      FA12760
     *     AMOL65(MXZMD),AMOL66(MXZMD),AMOL67(MXZMD),AMOL68(MXZMD),      FA12770
     *     ZST(MXZMD),PST(MXZMD),TST(MXZMD),AMOLS(MXZMD,MXMOL)           FA12780
      COMMON /MLATMC/ ATMNAM(6)                                          FA12790
      CHARACTER*24 ATMNAM                                                FA12800
C                                                                        FA12810
C     COMMON /TRAC/ TRAC(MXZMD,MXTRAC)                                   FA12820
C                                                                        FA12830
      COMMON /TRAC/ ANO(MXZMD),SO2(MXZMD),ANO2(MXZMD),ANH3(MXZMD),       FA12840
     *              HNO3(MXZMD),OH(MXZMD),HF(MXZMD),HCL(MXZMD),          FA12850
     *              HBR(MXZMD),HI(MXZMD),CLO(MXZMD),OCS(MXZMD),          FA12860
     *              H2CO(MXZMD),HOCL(MXZMD),AN2(MXZMD),HCN(MXZMD),       FA12870
     *              CH3CL(MXZMD),H2O2(MXZMD),C2H2(MXZMD),C2H6(MXZMD),    FA12880
     *              PH3(MXZMD),TDUM(MXZMD)                               FA12890
C                                                                        FA12900
      DATA ATMNAM(1) / 'TROPICAL                '/                       FA12910
      DATA ATMNAM(2) / 'MIDLATITUDE SUMMER      '/                       FA12920
      DATA ATMNAM(3) / 'MIDLATITUDE WINTER      '/                       FA12930
      DATA ATMNAM(4) / 'SUBARCTIC SUMMER        '/                       FA12940
      DATA ATMNAM(5) / 'SUBARCTIC WINTER        '/                       FA12950
      DATA ATMNAM(6) / 'U. S. STANDARD,  1976   '/                       FA12960
C                                                                        FA12970
C     DATA ALT (KM) /                                                    FA12980
C                                                                        FA12990
      DATA ALT /       0.0,       1.0,       2.0,       3.0,       4.0,  FA13000
     *                 5.0,       6.0,       7.0,       8.0,       9.0,  FA13010
     *                10.0,      11.0,      12.0,      13.0,      14.0,  FA13020
     *                15.0,      16.0,      17.0,      18.0,      19.0,  FA13030
     *                20.0,      21.0,      22.0,      23.0,      24.0,  FA13040
     *                25.0,      27.5,      30.0,      32.5,      35.0,  FA13050
     *                37.5,      40.0,      42.5,      45.0,      47.5,  FA13060
     *                50.0,      55.0,      60.0,      65.0,      70.0,  FA13070
     *                75.0,      80.0,      85.0,      90.0,      95.0,  FA13080
     *               100.0,     105.0,     110.0,     115.0,     120.0,  FA13090
     *               MXZ50*0.0 /                                         FA13100
C                                                                        FA13110
C     DATA PRESSURE /                                                    FA13120
C                                                                        FA13130
      DATA P1 /  1.013E+03, 9.040E+02, 8.050E+02, 7.150E+02, 6.330E+02,  FA13140
     *           5.590E+02, 4.920E+02, 4.320E+02, 3.780E+02, 3.290E+02,  FA13150
     *           2.860E+02, 2.470E+02, 2.130E+02, 1.820E+02, 1.560E+02,  FA13160
     *           1.320E+02, 1.110E+02, 9.370E+01, 7.890E+01, 6.660E+01,  FA13170
     *           5.650E+01, 4.800E+01, 4.090E+01, 3.500E+01, 3.000E+01,  FA13180
     *           2.570E+01, 1.763E+01, 1.220E+01, 8.520E+00, 6.000E+00,  FA13190
     *           4.260E+00, 3.050E+00, 2.200E+00, 1.590E+00, 1.160E+00,  FA13200
     *           8.540E-01, 4.560E-01, 2.390E-01, 1.210E-01, 5.800E-02,  FA13210
     *           2.600E-02, 1.100E-02, 4.400E-03, 1.720E-03, 6.880E-04,  FA13220
     *           2.890E-04, 1.300E-04, 6.470E-05, 3.600E-05, 2.250E-05,  FA13230
     *           MXZ50*0.0 /                                             FA13240
C                                                                        FA13250
      DATA P2 /  1.013E+03, 9.020E+02, 8.020E+02, 7.100E+02, 6.280E+02,  FA13260
     *           5.540E+02, 4.870E+02, 4.260E+02, 3.720E+02, 3.240E+02,  FA13270
     *           2.810E+02, 2.430E+02, 2.090E+02, 1.790E+02, 1.530E+02,  FA13280
     *           1.300E+02, 1.110E+02, 9.500E+01, 8.120E+01, 6.950E+01,  FA13290
     *           5.950E+01, 5.100E+01, 4.370E+01, 3.760E+01, 3.220E+01,  FA13300
     *           2.770E+01, 1.907E+01, 1.320E+01, 9.300E+00, 6.520E+00,  FA13310
     *           4.640E+00, 3.330E+00, 2.410E+00, 1.760E+00, 1.290E+00,  FA13320
     *           9.510E-01, 5.150E-01, 2.720E-01, 1.390E-01, 6.700E-02,  FA13330
     *           3.000E-02, 1.200E-02, 4.480E-03, 1.640E-03, 6.250E-04,  FA13340
     *           2.580E-04, 1.170E-04, 6.110E-05, 3.560E-05, 2.270E-05,  FA13350
     *           MXZ50*0.0 /                                             FA13360
C                                                                        FA13370
      DATA P3 /  1.018E+03, 8.973E+02, 7.897E+02, 6.938E+02, 6.081E+02,  FA13380
     *           5.313E+02, 4.627E+02, 4.016E+02, 3.473E+02, 2.993E+02,  FA13390
     *           2.568E+02, 2.199E+02, 1.882E+02, 1.611E+02, 1.378E+02,  FA13400
     *           1.178E+02, 1.007E+02, 8.610E+01, 7.360E+01, 6.280E+01,  FA13410
     *           5.370E+01, 4.580E+01, 3.910E+01, 3.340E+01, 2.860E+01,  FA13420
     *           2.440E+01, 1.646E+01, 1.110E+01, 7.560E+00, 5.180E+00,  FA13430
     *           3.600E+00, 2.530E+00, 1.800E+00, 1.290E+00, 9.400E-01,  FA13440
     *           6.830E-01, 3.620E-01, 1.880E-01, 9.500E-02, 4.700E-02,  FA13450
     *           2.220E-02, 1.030E-02, 4.560E-03, 1.980E-03, 8.770E-04,  FA13460
     *           4.074E-04, 2.000E-04, 1.057E-04, 5.980E-05, 3.600E-05,  FA13470
     *           MXZ50*0.0 /                                             FA13480
C                                                                        FA13490
      DATA P4 /  1.010E+03, 8.960E+02, 7.929E+02, 7.000E+02, 6.160E+02,  FA13500
     *           5.410E+02, 4.740E+02, 4.130E+02, 3.590E+02, 3.108E+02,  FA13510
     *           2.677E+02, 2.300E+02, 1.977E+02, 1.700E+02, 1.460E+02,  FA13520
     *           1.260E+02, 1.080E+02, 9.280E+01, 7.980E+01, 6.860E+01,  FA13530
     *           5.900E+01, 5.070E+01, 4.360E+01, 3.750E+01, 3.228E+01,  FA13540
     *           2.780E+01, 1.923E+01, 1.340E+01, 9.400E+00, 6.610E+00,  FA13550
     *           4.720E+00, 3.400E+00, 2.480E+00, 1.820E+00, 1.340E+00,  FA13560
     *           9.870E-01, 5.370E-01, 2.880E-01, 1.470E-01, 7.100E-02,  FA13570
     *           3.200E-02, 1.250E-02, 4.510E-03, 1.610E-03, 6.060E-04,  FA13580
     *           2.480E-04, 1.130E-04, 6.000E-05, 3.540E-05, 2.260E-05,  FA13590
     *           MXZ50*0.0 /                                             FA13600
C                                                                        FA13610
      DATA P5 /  1.013E+03, 8.878E+02, 7.775E+02, 6.798E+02, 5.932E+02,  FA13620
     *           5.158E+02, 4.467E+02, 3.853E+02, 3.308E+02, 2.829E+02,  FA13630
     *           2.418E+02, 2.067E+02, 1.766E+02, 1.510E+02, 1.291E+02,  FA13640
     *           1.103E+02, 9.431E+01, 8.058E+01, 6.882E+01, 5.875E+01,  FA13650
     *           5.014E+01, 4.277E+01, 3.647E+01, 3.109E+01, 2.649E+01,  FA13660
     *           2.256E+01, 1.513E+01, 1.020E+01, 6.910E+00, 4.701E+00,  FA13670
     *           3.230E+00, 2.243E+00, 1.570E+00, 1.113E+00, 7.900E-01,  FA13680
     *           5.719E-01, 2.990E-01, 1.550E-01, 7.900E-02, 4.000E-02,  FA13690
     *           2.000E-02, 9.660E-03, 4.500E-03, 2.022E-03, 9.070E-04,  FA13700
     *           4.230E-04, 2.070E-04, 1.080E-04, 6.000E-05, 3.590E-05,  FA13710
     *           MXZ50*0.0 /                                             FA13720
C                                                                        FA13730
      DATA P6 /  1.013E+03, 8.988E+02, 7.950E+02, 7.012E+02, 6.166E+02,  FA13740
     *           5.405E+02, 4.722E+02, 4.111E+02, 3.565E+02, 3.080E+02,  FA13750
     *           2.650E+02, 2.270E+02, 1.940E+02, 1.658E+02, 1.417E+02,  FA13760
     *           1.211E+02, 1.035E+02, 8.850E+01, 7.565E+01, 6.467E+01,  FA13770
     *           5.529E+01, 4.729E+01, 4.047E+01, 3.467E+01, 2.972E+01,  FA13780
     *           2.549E+01, 1.743E+01, 1.197E+01, 8.258E+00, 5.746E+00,  FA13790
     *           4.041E+00, 2.871E+00, 2.060E+00, 1.491E+00, 1.090E+00,  FA13800
     *           7.978E-01, 4.250E-01, 2.190E-01, 1.090E-01, 5.220E-02,  FA13810
     *           2.400E-02, 1.050E-02, 4.460E-03, 1.840E-03, 7.600E-04,  FA13820
     *           3.200E-04, 1.450E-04, 7.100E-05, 4.010E-05, 2.540E-05,  FA13830
     *           MXZ50*0.0 /                                             FA13840
C                                                                        FA13850
C     DATA TEMPERATURE /                                                 FA13860
C                                                                        FA13870
      DATA T1 /     299.70,    293.70,    287.70,    283.70,    277.00,  FA13880
     *              270.30,    263.60,    257.00,    250.30,    243.60,  FA13890
     *              237.00,    230.10,    223.60,    217.00,    210.30,  FA13900
     *              203.70,    197.00,    194.80,    198.80,    202.70,  FA13910
     *              206.70,    210.70,    214.60,    217.00,    219.20,  FA13920
     *              221.40,    227.00,    232.30,    237.70,    243.10,  FA13930
     *              248.50,    254.00,    259.40,    264.80,    269.60,  FA13940
     *              270.20,    263.40,    253.10,    236.00,    218.90,  FA13950
     *              201.80,    184.80,    177.10,    177.00,    184.30,  FA13960
     *              190.70,    212.00,    241.60,    299.70,    380.00,  FA13970
     *              MXZ50*0.0 /                                          FA13980
C                                                                        FA13990
      DATA T2 /     294.20,    289.70,    285.20,    279.20,    273.20,  FA14000
     *              267.20,    261.20,    254.70,    248.20,    241.70,  FA14010
     *              235.30,    228.80,    222.30,    215.80,    215.70,  FA14020
     *              215.70,    215.70,    215.70,    216.80,    217.90,  FA14030
     *              219.20,    220.40,    221.60,    222.80,    223.90,  FA14040
     *              225.10,    228.45,    233.70,    239.00,    245.20,  FA14050
     *              251.30,    257.50,    263.70,    269.90,    275.20,  FA14060
     *              275.70,    269.30,    257.10,    240.10,    218.10,  FA14070
     *              196.10,    174.10,    165.10,    165.00,    178.30,  FA14080
     *              190.50,    222.20,    262.40,    316.80,    380.00,  FA14090
     *              MXZ50*0.0 /                                          FA14100
C                                                                        FA14110
      DATA T3 /     272.20,    268.70,    265.20,    261.70,    255.70,  FA14120
     *              249.70,    243.70,    237.70,    231.70,    225.70,  FA14130
     *              219.70,    219.20,    218.70,    218.20,    217.70,  FA14140
     *              217.20,    216.70,    216.20,    215.70,    215.20,  FA14150
     *              215.20,    215.20,    215.20,    215.20,    215.20,  FA14160
     *              215.20,    215.50,    217.40,    220.40,    227.90,  FA14170
     *              235.50,    243.20,    250.80,    258.50,    265.10,  FA14180
     *              265.70,    260.60,    250.80,    240.90,    230.70,  FA14190
     *              220.40,    210.10,    199.80,    199.50,    208.30,  FA14200
     *              218.60,    237.10,    259.50,    293.00,    333.00,  FA14210
     *              MXZ50*0.0 /                                          FA14220
C                                                                        FA14230
      DATA T4 /     287.20,    281.70,    276.30,    270.90,    265.50,  FA14240
     *              260.10,    253.10,    246.10,    239.20,    232.20,  FA14250
     *              225.20,    225.20,    225.20,    225.20,    225.20,  FA14260
     *              225.20,    225.20,    225.20,    225.20,    225.20,  FA14270
     *              225.20,    225.20,    225.20,    225.20,    226.60,  FA14280
     *              228.10,    231.00,    235.10,    240.00,    247.20,  FA14290
     *              254.60,    262.10,    269.50,    273.60,    276.20,  FA14300
     *              277.20,    274.00,    262.70,    239.70,    216.60,  FA14310
     *              193.60,    170.60,    161.70,    161.60,    176.80,  FA14320
     *              190.40,    226.00,    270.10,    322.70,    380.00,  FA14330
     *              MXZ50*0.0 /                                          FA14340
C                                                                        FA14350
      DATA T5 /     257.20,    259.10,    255.90,    252.70,    247.70,  FA14360
     *              240.90,    234.10,    227.30,    220.60,    217.20,  FA14370
     *              217.20,    217.20,    217.20,    217.20,    217.20,  FA14380
     *              217.20,    216.60,    216.00,    215.40,    214.80,  FA14390
     *              214.20,    213.60,    213.00,    212.40,    211.80,  FA14400
     *              211.20,    213.60,    216.00,    218.50,    222.30,  FA14410
     *              228.50,    234.70,    240.80,    247.00,    253.20,  FA14420
     *              259.30,    259.10,    250.90,    248.40,    245.40,  FA14430
     *              234.70,    223.90,    213.10,    202.30,    211.00,  FA14440
     *              218.50,    234.00,    252.60,    288.50,    333.00,  FA14450
     *              MXZ50*0.0 /                                          FA14460
C                                                                        FA14470
      DATA T6 /     288.20,    281.70,    275.20,    268.70,    262.20,  FA14480
     *              255.70,    249.20,    242.70,    236.20,    229.70,  FA14490
     *              223.30,    216.80,    216.70,    216.70,    216.70,  FA14500
     *              216.70,    216.70,    216.70,    216.70,    216.70,  FA14510
     *              216.70,    217.60,    218.60,    219.60,    220.60,  FA14520
     *              221.60,    224.00,    226.50,    229.60,    236.50,  FA14530
     *              243.40,    250.40,    257.30,    264.20,    270.60,  FA14540
     *              270.70,    260.80,    247.00,    233.30,    219.60,  FA14550
     *              208.40,    198.60,    188.90,    186.90,    188.40,  FA14560
     *              195.10,    208.80,    240.00,    300.00,    360.00,  FA14570
     *              MXZ50*0.0 /                                          FA14580
C                                                                        FA14590
C     DATA  H2O /                                                        FA14600
C                                                                        FA14610
      DATA AMOL11 /                                                      FA14620
     *           2.593E+04, 1.949E+04, 1.534E+04, 8.600E+03, 4.441E+03,  FA14630
     *           3.346E+03, 2.101E+03, 1.289E+03, 7.637E+02, 4.098E+02,  FA14640
     *           1.912E+02, 7.306E+01, 2.905E+01, 9.900E+00, 6.220E+00,  FA14650
     *           4.000E+00, 3.000E+00, 2.900E+00, 2.750E+00, 2.600E+00,  FA14660
     *           2.600E+00, 2.650E+00, 2.800E+00, 2.900E+00, 3.200E+00,  FA14670
     *           3.250E+00, 3.600E+00, 4.000E+00, 4.300E+00, 4.600E+00,  FA14680
     *           4.900E+00, 5.200E+00, 5.500E+00, 5.700E+00, 5.900E+00,  FA14690
     *           6.000E+00, 6.000E+00, 6.000E+00, 5.400E+00, 4.500E+00,  FA14700
     *           3.300E+00, 2.100E+00, 1.300E+00, 8.500E-01, 5.400E-01,  FA14710
     *           4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01,  FA14720
     *           MXZ50*0.0 /                                             FA14730
C                                                                        FA14740
      DATA AMOL21 /                                                      FA14750
     *           1.876E+04, 1.378E+04, 9.680E+03, 5.984E+03, 3.813E+03,  FA14760
     *           2.225E+03, 1.510E+03, 1.020E+03, 6.464E+02, 4.129E+02,  FA14770
     *           2.472E+02, 9.556E+01, 2.944E+01, 8.000E+00, 5.000E+00,  FA14780
     *           3.400E+00, 3.300E+00, 3.200E+00, 3.150E+00, 3.200E+00,  FA14790
     *           3.300E+00, 3.450E+00, 3.600E+00, 3.850E+00, 4.000E+00,  FA14800
     *           4.200E+00, 4.450E+00, 4.700E+00, 4.850E+00, 4.950E+00,  FA14810
     *           5.000E+00, 5.100E+00, 5.300E+00, 5.450E+00, 5.500E+00,  FA14820
     *           5.500E+00, 5.350E+00, 5.000E+00, 4.400E+00, 3.700E+00,  FA14830
     *           2.950E+00, 2.100E+00, 1.330E+00, 8.500E-01, 5.400E-01,  FA14840
     *           4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01,  FA14850
     *           MXZ50*0.0 /                                             FA14860
C                                                                        FA14870
      DATA AMOL31 /                                                      FA14880
     *           4.316E+03, 3.454E+03, 2.788E+03, 2.088E+03, 1.280E+03,  FA14890
     *           8.241E+02, 5.103E+02, 2.321E+02, 1.077E+02, 5.566E+01,  FA14900
     *           2.960E+01, 1.000E+01, 6.000E+00, 5.000E+00, 4.800E+00,  FA14910
     *           4.700E+00, 4.600E+00, 4.500E+00, 4.500E+00, 4.500E+00,  FA14920
     *           4.500E+00, 4.500E+00, 4.530E+00, 4.550E+00, 4.600E+00,  FA14930
     *           4.650E+00, 4.700E+00, 4.750E+00, 4.800E+00, 4.850E+00,  FA14940
     *           4.900E+00, 4.950E+00, 5.000E+00, 5.000E+00, 5.000E+00,  FA14950
     *           4.950E+00, 4.850E+00, 4.500E+00, 4.000E+00, 3.300E+00,  FA14960
     *           2.700E+00, 2.000E+00, 1.330E+00, 8.500E-01, 5.400E-01,  FA14970
     *           4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01,  FA14980
     *           MXZ50*0.0 /                                             FA14990
C                                                                        FA15000
      DATA AMOL41 /                                                      FA15010
     *           1.194E+04, 8.701E+03, 6.750E+03, 4.820E+03, 3.380E+03,  FA15020
     *           2.218E+03, 1.330E+03, 7.971E+02, 3.996E+02, 1.300E+02,  FA15030
     *           4.240E+01, 1.330E+01, 6.000E+00, 4.450E+00, 4.000E+00,  FA15040
     *           4.000E+00, 4.000E+00, 4.050E+00, 4.300E+00, 4.500E+00,  FA15050
     *           4.600E+00, 4.700E+00, 4.800E+00, 4.830E+00, 4.850E+00,  FA15060
     *           4.900E+00, 4.950E+00, 5.000E+00, 5.000E+00, 5.000E+00,  FA15070
     *           5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00,  FA15080
     *           4.950E+00, 4.850E+00, 4.500E+00, 4.000E+00, 3.300E+00,  FA15090
     *           2.700E+00, 2.000E+00, 1.330E+00, 8.500E-01, 5.400E-01,  FA15100
     *           4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01,  FA15110
     *           MXZ50*0.0 /                                             FA15120
C                                                                        FA15130
      DATA AMOL51 /                                                      FA15140
     *           1.405E+03, 1.615E+03, 1.427E+03, 1.166E+03, 7.898E+02,  FA15150
     *           4.309E+02, 2.369E+02, 1.470E+02, 3.384E+01, 2.976E+01,  FA15160
     *           2.000E+01, 1.000E+01, 6.000E+00, 4.450E+00, 4.500E+00,  FA15170
     *           4.550E+00, 4.600E+00, 4.650E+00, 4.700E+00, 4.750E+00,  FA15180
     *           4.800E+00, 4.850E+00, 4.900E+00, 4.950E+00, 5.000E+00,  FA15190
     *           5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00,  FA15200
     *           5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00,  FA15210
     *           4.950E+00, 4.850E+00, 4.500E+00, 4.000E+00, 3.300E+00,  FA15220
     *           2.700E+00, 2.000E+00, 1.330E+00, 8.500E-01, 5.400E-01,  FA15230
     *           4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01,  FA15240
     *           MXZ50*0.0 /                                             FA15250
C                                                                        FA15260
      DATA AMOL61 /                                                      FA15270
     *           7.745E+03, 6.071E+03, 4.631E+03, 3.182E+03, 2.158E+03,  FA15280
     *           1.397E+03, 9.254E+02, 5.720E+02, 3.667E+02, 1.583E+02,  FA15290
     *           6.996E+01, 3.613E+01, 1.906E+01, 1.085E+01, 5.927E+00,  FA15300
     *           5.000E+00, 3.950E+00, 3.850E+00, 3.825E+00, 3.850E+00,  FA15310
     *           3.900E+00, 3.975E+00, 4.065E+00, 4.200E+00, 4.300E+00,  FA15320
     *           4.425E+00, 4.575E+00, 4.725E+00, 4.825E+00, 4.900E+00,  FA15330
     *           4.950E+00, 5.025E+00, 5.150E+00, 5.225E+00, 5.250E+00,  FA15340
     *           5.225E+00, 5.100E+00, 4.750E+00, 4.200E+00, 3.500E+00,  FA15350
     *           2.825E+00, 2.050E+00, 1.330E+00, 8.500E-01, 5.400E-01,  FA15360
     *           4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01,  FA15370
     *           MXZ50*0.0 /                                             FA15380
C                                                                        FA15390
C     DATA CO2 /                                                         FA15400
C                                                                        FA15410
      DATA AMOL12 /                                                      FA15420
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15430
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15440
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15450
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15460
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15470
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15480
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15490
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15500
     *           3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,  FA15510
     *           1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01,  FA15520
     *           MXZ50*0.0 /                                             FA15530
C                                                                        FA15540
      DATA AMOL22 /                                                      FA15550
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15560
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15570
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15580
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15590
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15600
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15610
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15620
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15630
     *           3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,  FA15640
     *           1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01,  FA15650
     *           MXZ50*0.0 /                                             FA15660
C                                                                        FA15670
      DATA AMOL32 /                                                      FA15680
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15690
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15700
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15710
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15720
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15730
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15740
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15750
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15760
     *           3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,  FA15770
     *           1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01,  FA15780
     *           MXZ50*0.0 /                                             FA15790
C                                                                        FA15800
      DATA AMOL42 /                                                      FA15810
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15820
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15830
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15840
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15850
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15860
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15870
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15880
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15890
     *           3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,  FA15900
     *           1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01,  FA15910
     *           MXZ50*0.0 /                                             FA15920
C                                                                        FA15930
      DATA AMOL52 /                                                      FA15940
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15950
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15960
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15970
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15980
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA15990
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16000
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16010
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16020
     *           3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,  FA16030
     *           1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01,  FA16040
     *           MXZ50*0.0 /                                             FA16050
C                                                                        FA16060
      DATA AMOL62 /                                                      FA16070
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16080
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16090
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16100
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16110
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16120
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16130
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16140
     *           3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,  FA16150
     *           3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,  FA16160
     *           1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01,  FA16170
     *           MXZ50*0.0 /                                             FA16180
C                                                                        FA16190
C     DATA OZONE /                                                       FA16200
C                                                                        FA16210
      DATA AMOL13 /                                                      FA16220
     *           2.869E-02, 3.150E-02, 3.342E-02, 3.504E-02, 3.561E-02,  FA16230
     *           3.767E-02, 3.989E-02, 4.223E-02, 4.471E-02, 5.000E-02,  FA16240
     *           5.595E-02, 6.613E-02, 7.815E-02, 9.289E-02, 1.050E-01,  FA16250
     *           1.256E-01, 1.444E-01, 2.500E-01, 5.000E-01, 9.500E-01,  FA16260
     *           1.400E+00, 1.800E+00, 2.400E+00, 3.400E+00, 4.300E+00,  FA16270
     *           5.400E+00, 7.800E+00, 9.300E+00, 9.850E+00, 9.700E+00,  FA16280
     *           8.800E+00, 7.500E+00, 5.900E+00, 4.500E+00, 3.450E+00,  FA16290
     *           2.800E+00, 1.800E+00, 1.100E+00, 6.500E-01, 3.000E-01,  FA16300
     *           1.800E-01, 3.300E-01, 5.000E-01, 5.200E-01, 5.000E-01,  FA16310
     *           4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04,  FA16320
     *           MXZ50*0.0 /                                             FA16330
C                                                                        FA16340
      DATA AMOL23 /                                                      FA16350
     *           3.017E-02, 3.337E-02, 3.694E-02, 4.222E-02, 4.821E-02,  FA16360
     *           5.512E-02, 6.408E-02, 7.764E-02, 9.126E-02, 1.111E-01,  FA16370
     *           1.304E-01, 1.793E-01, 2.230E-01, 3.000E-01, 4.400E-01,  FA16380
     *           5.000E-01, 6.000E-01, 7.000E-01, 1.000E+00, 1.500E+00,  FA16390
     *           2.000E+00, 2.400E+00, 2.900E+00, 3.400E+00, 4.000E+00,  FA16400
     *           4.800E+00, 6.000E+00, 7.000E+00, 8.100E+00, 8.900E+00,  FA16410
     *           8.700E+00, 7.550E+00, 5.900E+00, 4.500E+00, 3.500E+00,  FA16420
     *           2.800E+00, 1.800E+00, 1.300E+00, 8.000E-01, 4.000E-01,  FA16430
     *           1.900E-01, 2.000E-01, 5.700E-01, 7.500E-01, 7.000E-01,  FA16440
     *           4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04,  FA16450
     *           MXZ50*0.0 /                                             FA16460
C                                                                        FA16470
      DATA AMOL33 /                                                      FA16480
     *           2.778E-02, 2.800E-02, 2.849E-02, 3.200E-02, 3.567E-02,  FA16490
     *           4.720E-02, 5.837E-02, 7.891E-02, 1.039E-01, 1.567E-01,  FA16500
     *           2.370E-01, 3.624E-01, 5.232E-01, 7.036E-01, 8.000E-01,  FA16510
     *           9.000E-01, 1.100E+00, 1.400E+00, 1.800E+00, 2.300E+00,  FA16520
     *           2.900E+00, 3.500E+00, 3.900E+00, 4.300E+00, 4.700E+00,  FA16530
     *           5.100E+00, 5.600E+00, 6.100E+00, 6.800E+00, 7.100E+00,  FA16540
     *           7.200E+00, 6.900E+00, 5.900E+00, 4.600E+00, 3.700E+00,  FA16550
     *           2.750E+00, 1.700E+00, 1.000E-00, 5.500E-01, 3.200E-01,  FA16560
     *           2.500E-01, 2.300E-01, 5.500E-01, 8.000E-01, 8.000E-01,  FA16570
     *           4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04,  FA16580
     *           MXZ50*0.0 /                                             FA16590
C                                                                        FA16600
      DATA AMOL43 /                                                      FA16610
     *           2.412E-02, 2.940E-02, 3.379E-02, 3.887E-02, 4.478E-02,  FA16620
     *           5.328E-02, 6.564E-02, 7.738E-02, 9.114E-02, 1.420E-01,  FA16630
     *           1.890E-01, 3.050E-01, 4.100E-01, 5.000E-01, 6.000E-01,  FA16640
     *           7.000E-01, 8.500E-01, 1.000E+00, 1.300E+00, 1.700E+00,  FA16650
     *           2.100E+00, 2.700E+00, 3.300E+00, 3.700E+00, 4.200E+00,  FA16660
     *           4.500E+00, 5.300E+00, 5.700E+00, 6.900E+00, 7.700E+00,  FA16670
     *           7.800E+00, 7.000E+00, 5.400E+00, 4.200E+00, 3.200E+00,  FA16680
     *           2.500E+00, 1.700E+00, 1.200E+00, 8.000E-01, 4.000E-01,  FA16690
     *           2.000E-01, 1.800E-01, 6.500E-01, 9.000E-01, 8.000E-01,  FA16700
     *           4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04,  FA16710
     *           MXZ50*0.0 /                                             FA16720
C                                                                        FA16730
      DATA AMOL53 /                                                      FA16740
     *           1.802E-02, 2.072E-02, 2.336E-02, 2.767E-02, 3.253E-02,  FA16750
     *           3.801E-02, 4.446E-02, 7.252E-02, 1.040E-01, 2.100E-01,  FA16760
     *           3.000E-01, 3.500E-01, 4.000E-01, 6.500E-01, 9.000E-01,  FA16770
     *           1.200E+00, 1.500E+00, 1.900E+00, 2.450E+00, 3.100E+00,  FA16780
     *           3.700E+00, 4.000E+00, 4.200E+00, 4.500E+00, 4.600E+00,  FA16790
     *           4.700E+00, 4.900E+00, 5.400E+00, 5.900E+00, 6.200E+00,  FA16800
     *           6.250E+00, 5.900E+00, 5.100E+00, 4.100E+00, 3.000E+00,  FA16810
     *           2.600E+00, 1.600E+00, 9.500E-01, 6.500E-01, 5.000E-01,  FA16820
     *           3.300E-01, 1.300E-01, 7.500E-01, 8.000E-01, 8.000E-01,  FA16830
     *           4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04,  FA16840
     *           MXZ50*0.0 /                                             FA16850
C                                                                        FA16860
      DATA AMOL63 /                                                      FA16870
     *           2.660E-02, 2.931E-02, 3.237E-02, 3.318E-02, 3.387E-02,  FA16880
     *           3.768E-02, 4.112E-02, 5.009E-02, 5.966E-02, 9.168E-02,  FA16890
     *           1.313E-01, 2.149E-01, 3.095E-01, 3.846E-01, 5.030E-01,  FA16900
     *           6.505E-01, 8.701E-01, 1.187E+00, 1.587E+00, 2.030E+00,  FA16910
     *           2.579E+00, 3.028E+00, 3.647E+00, 4.168E+00, 4.627E+00,  FA16920
     *           5.118E+00, 5.803E+00, 6.553E+00, 7.373E+00, 7.837E+00,  FA16930
     *           7.800E+00, 7.300E+00, 6.200E+00, 5.250E+00, 4.100E+00,  FA16940
     *           3.100E+00, 1.800E+00, 1.100E+00, 7.000E-01, 3.000E-01,  FA16950
     *           2.500E-01, 3.000E-01, 5.000E-01, 7.000E-01, 7.000E-01,  FA16960
     *           4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04,  FA16970
     *           MXZ50*0.0 /                                             FA16980
C                                                                        FA16990
C     DATA  N2O /                                                        FA17000
C                                                                        FA17010
      DATA AMOL14 /                                                      FA17020
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,  FA17030
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01,  FA17040
     *           3.179E-01, 3.140E-01, 3.095E-01, 3.048E-01, 2.999E-01,  FA17050
     *           2.944E-01, 2.877E-01, 2.783E-01, 2.671E-01, 2.527E-01,  FA17060
     *           2.365E-01, 2.194E-01, 2.051E-01, 1.967E-01, 1.875E-01,  FA17070
     *           1.756E-01, 1.588E-01, 1.416E-01, 1.165E-01, 9.275E-02,  FA17080
     *           6.693E-02, 4.513E-02, 2.751E-02, 1.591E-02, 9.378E-03,  FA17090
     *           4.752E-03, 3.000E-03, 2.065E-03, 1.507E-03, 1.149E-03,  FA17100
     *           8.890E-04, 7.056E-04, 5.716E-04, 4.708E-04, 3.932E-04,  FA17110
     *           3.323E-04, 2.837E-04, 2.443E-04, 2.120E-04, 1.851E-04,  FA17120
     *           MXZ50*0.0 /                                             FA17130
C                                                                        FA17140
      DATA AMOL24 /                                                      FA17150
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,  FA17160
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01, 3.163E-01,  FA17170
     *           3.096E-01, 2.989E-01, 2.936E-01, 2.860E-01, 2.800E-01,  FA17180
     *           2.724E-01, 2.611E-01, 2.421E-01, 2.174E-01, 1.843E-01,  FA17190
     *           1.607E-01, 1.323E-01, 1.146E-01, 1.035E-01, 9.622E-02,  FA17200
     *           8.958E-02, 8.006E-02, 6.698E-02, 4.958E-02, 3.695E-02,  FA17210
     *           2.519E-02, 1.736E-02, 1.158E-02, 7.665E-03, 5.321E-03,  FA17220
     *           3.215E-03, 2.030E-03, 1.397E-03, 1.020E-03, 7.772E-04,  FA17230
     *           6.257E-04, 5.166E-04, 4.352E-04, 3.727E-04, 3.237E-04,  FA17240
     *           2.844E-04, 2.524E-04, 2.260E-04, 2.039E-04, 1.851E-04,  FA17250
     *           MXZ50*0.0 /                                             FA17260
C                                                                        FA17270
      DATA AMOL34 /                                                      FA17280
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,  FA17290
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01, 3.163E-01,  FA17300
     *           3.096E-01, 2.989E-01, 2.936E-01, 2.860E-01, 2.800E-01,  FA17310
     *           2.724E-01, 2.611E-01, 2.421E-01, 2.174E-01, 1.843E-01,  FA17320
     *           1.621E-01, 1.362E-01, 1.230E-01, 1.124E-01, 1.048E-01,  FA17330
     *           9.661E-02, 8.693E-02, 7.524E-02, 6.126E-02, 5.116E-02,  FA17340
     *           3.968E-02, 2.995E-02, 2.080E-02, 1.311E-02, 8.071E-03,  FA17350
     *           4.164E-03, 2.629E-03, 1.809E-03, 1.321E-03, 1.007E-03,  FA17360
     *           7.883E-04, 6.333E-04, 5.194E-04, 4.333E-04, 3.666E-04,  FA17370
     *           3.140E-04, 2.717E-04, 2.373E-04, 2.089E-04, 1.851E-04,  FA17380
     *           MXZ50*0.0 /                                             FA17390
C                                                                        FA17400
      DATA AMOL44 /                                                      FA17410
     *           3.100E-01, 3.100E-01, 3.100E-01, 3.100E-01, 3.079E-01,  FA17420
     *           3.024E-01, 2.906E-01, 2.822E-01, 2.759E-01, 2.703E-01,  FA17430
     *           2.651E-01, 2.600E-01, 2.549E-01, 2.494E-01, 2.433E-01,  FA17440
     *           2.355E-01, 2.282E-01, 2.179E-01, 2.035E-01, 1.817E-01,  FA17450
     *           1.567E-01, 1.350E-01, 1.218E-01, 1.102E-01, 9.893E-02,  FA17460
     *           8.775E-02, 7.327E-02, 5.941E-02, 4.154E-02, 3.032E-02,  FA17470
     *           1.949E-02, 1.274E-02, 9.001E-03, 6.286E-03, 4.558E-03,  FA17480
     *           2.795E-03, 1.765E-03, 1.214E-03, 8.866E-04, 6.756E-04,  FA17490
     *           5.538E-04, 4.649E-04, 3.979E-04, 3.459E-04, 3.047E-04,  FA17500
     *           2.713E-04, 2.439E-04, 2.210E-04, 2.017E-04, 1.851E-04,  FA17510
     *           MXZ50*0.0 /                                             FA17520
C                                                                        FA17530
      DATA AMOL54 /                                                      FA17540
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,  FA17550
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01, 3.163E-01,  FA17560
     *           3.096E-01, 2.989E-01, 2.936E-01, 2.860E-01, 2.800E-01,  FA17570
     *           2.724E-01, 2.611E-01, 2.421E-01, 2.174E-01, 1.843E-01,  FA17580
     *           1.621E-01, 1.362E-01, 1.230E-01, 1.122E-01, 1.043E-01,  FA17590
     *           9.570E-02, 8.598E-02, 7.314E-02, 5.710E-02, 4.670E-02,  FA17600
     *           3.439E-02, 2.471E-02, 1.631E-02, 1.066E-02, 7.064E-03,  FA17610
     *           3.972E-03, 2.508E-03, 1.726E-03, 1.260E-03, 9.602E-04,  FA17620
     *           7.554E-04, 6.097E-04, 5.024E-04, 4.210E-04, 3.579E-04,  FA17630
     *           3.080E-04, 2.678E-04, 2.350E-04, 2.079E-04, 1.851E-04,  FA17640
     *           MXZ50*0.0 /                                             FA17650
C                                                                        FA17660
      DATA AMOL64 /                                                      FA17670
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,  FA17680
     *           3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01,  FA17690
     *           3.179E-01, 3.140E-01, 3.095E-01, 3.048E-01, 2.999E-01,  FA17700
     *           2.944E-01, 2.877E-01, 2.783E-01, 2.671E-01, 2.527E-01,  FA17710
     *           2.365E-01, 2.194E-01, 2.051E-01, 1.967E-01, 1.875E-01,  FA17720
     *           1.756E-01, 1.588E-01, 1.416E-01, 1.165E-01, 9.275E-02,  FA17730
     *           6.693E-02, 4.513E-02, 2.751E-02, 1.591E-02, 9.378E-03,  FA17740
     *           4.752E-03, 3.000E-03, 2.065E-03, 1.507E-03, 1.149E-03,  FA17750
     *           8.890E-04, 7.056E-04, 5.716E-04, 4.708E-04, 3.932E-04,  FA17760
     *           3.323E-04, 2.837E-04, 2.443E-04, 2.120E-04, 1.851E-04,  FA17770
     *           MXZ50*0.0 /                                             FA17780
C                                                                        FA17790
C     DATA CO /                                                          FA17800
C                                                                        FA17810
      DATA AMOL15 /                                                      FA17820
     *           1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,  FA17830
     *           1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,  FA17840
     *           9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,  FA17850
     *           3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,  FA17860
     *           1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,  FA17870
     *           1.521E-02, 1.722E-02, 1.995E-02, 2.266E-02, 2.487E-02,  FA17880
     *           2.738E-02, 3.098E-02, 3.510E-02, 3.987E-02, 4.482E-02,  FA17890
     *           5.092E-02, 5.985E-02, 6.960E-02, 9.188E-02, 1.938E-01,  FA17900
     *           5.688E-01, 1.549E+00, 3.849E+00, 6.590E+00, 1.044E+01,  FA17910
     *           1.705E+01, 2.471E+01, 3.358E+01, 4.148E+01, 5.000E+01,  FA17920
     *           MXZ50*0.0 /                                             FA17930
C                                                                        FA17940
      DATA AMOL25 /                                                      FA17950
     *           1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,  FA17960
     *           1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,  FA17970
     *           9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,  FA17980
     *           3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,  FA17990
     *           1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,  FA18000
     *           1.521E-02, 1.722E-02, 1.995E-02, 2.266E-02, 2.487E-02,  FA18010
     *           2.716E-02, 2.962E-02, 3.138E-02, 3.307E-02, 3.487E-02,  FA18020
     *           3.645E-02, 3.923E-02, 4.673E-02, 6.404E-02, 1.177E-01,  FA18030
     *           2.935E-01, 6.815E-01, 1.465E+00, 2.849E+00, 5.166E+00,  FA18040
     *           1.008E+01, 1.865E+01, 2.863E+01, 3.890E+01, 5.000E+01,  FA18050
     *           MXZ50*0.0 /                                             FA18060
C                                                                        FA18070
      DATA AMOL35 /                                                      FA18080
     *           1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,  FA18090
     *           1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,  FA18100
     *           9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,  FA18110
     *           3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,  FA18120
     *           1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,  FA18130
     *           1.498E-02, 1.598E-02, 1.710E-02, 1.850E-02, 1.997E-02,  FA18140
     *           2.147E-02, 2.331E-02, 2.622E-02, 3.057E-02, 3.803E-02,  FA18150
     *           6.245E-02, 1.480E-01, 2.926E-01, 5.586E-01, 1.078E+00,  FA18160
     *           1.897E+00, 2.960E+00, 4.526E+00, 6.862E+00, 1.054E+01,  FA18170
     *           1.709E+01, 2.473E+01, 3.359E+01, 4.149E+01, 5.000E+01,  FA18180
     *           MXZ50*0.0 /                                             FA18190
C                                                                        FA18200
      DATA AMOL45 /                                                      FA18210
     *           1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,  FA18220
     *           1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,  FA18230
     *           9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,  FA18240
     *           3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,  FA18250
     *           1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,  FA18260
     *           1.510E-02, 1.649E-02, 1.808E-02, 1.997E-02, 2.183E-02,  FA18270
     *           2.343E-02, 2.496E-02, 2.647E-02, 2.809E-02, 2.999E-02,  FA18280
     *           3.220E-02, 3.650E-02, 4.589E-02, 6.375E-02, 1.176E-01,  FA18290
     *           3.033E-01, 7.894E-01, 1.823E+00, 3.402E+00, 5.916E+00,  FA18300
     *           1.043E+01, 1.881E+01, 2.869E+01, 3.892E+01, 5.000E+01,  FA18310
     *           MXZ50*0.0 /                                             FA18320
C                                                                        FA18330
      DATA AMOL55 /                                                      FA18340
     *           1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,  FA18350
     *           1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,  FA18360
     *           9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,  FA18370
     *           3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,  FA18380
     *           1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,  FA18390
     *           1.521E-02, 1.722E-02, 2.037E-02, 2.486E-02, 3.168E-02,  FA18400
     *           4.429E-02, 6.472E-02, 1.041E-01, 1.507E-01, 2.163E-01,  FA18410
     *           3.141E-01, 4.842E-01, 7.147E-01, 1.067E+00, 1.516E+00,  FA18420
     *           2.166E+00, 3.060E+00, 4.564E+00, 6.877E+00, 1.055E+01,  FA18430
     *           1.710E+01, 2.473E+01, 3.359E+01, 4.149E+01, 5.000E+01,  FA18440
     *           MXZ50*0.0 /                                             FA18450
C                                                                        FA18460
      DATA AMOL65 /                                                      FA18470
     *           1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,  FA18480
     *           1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,  FA18490
     *           9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,  FA18500
     *           3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,  FA18510
     *           1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,  FA18520
     *           1.498E-02, 1.598E-02, 1.710E-02, 1.850E-02, 2.009E-02,  FA18530
     *           2.220E-02, 2.497E-02, 2.824E-02, 3.241E-02, 3.717E-02,  FA18540
     *           4.597E-02, 6.639E-02, 1.073E-01, 1.862E-01, 3.059E-01,  FA18550
     *           6.375E-01, 1.497E+00, 3.239E+00, 5.843E+00, 1.013E+01,  FA18560
     *           1.692E+01, 2.467E+01, 3.356E+01, 4.148E+01, 5.000E+01,  FA18570
     *           MXZ50*0.0 /                                             FA18580
C                                                                        FA18590
C     DATA  CH4 /                                                        FA18600
C                                                                        FA18610
      DATA AMOL16 /                                                      FA18620
     *           1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00,  FA18630
     *           1.700E+00, 1.700E+00, 1.699E+00, 1.697E+00, 1.693E+00,  FA18640
     *           1.685E+00, 1.675E+00, 1.662E+00, 1.645E+00, 1.626E+00,  FA18650
     *           1.605E+00, 1.582E+00, 1.553E+00, 1.521E+00, 1.480E+00,  FA18660
     *           1.424E+00, 1.355E+00, 1.272E+00, 1.191E+00, 1.118E+00,  FA18670
     *           1.055E+00, 9.870E-01, 9.136E-01, 8.300E-01, 7.460E-01,  FA18680
     *           6.618E-01, 5.638E-01, 4.614E-01, 3.631E-01, 2.773E-01,  FA18690
     *           2.100E-01, 1.651E-01, 1.500E-01, 1.500E-01, 1.500E-01,  FA18700
     *           1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,  FA18710
     *           1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02,  FA18720
     *           MXZ50*0.0 /                                             FA18730
C                                                                        FA18740
      DATA AMOL26 /                                                      FA18750
     *           1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.697E+00,  FA18760
     *           1.687E+00, 1.672E+00, 1.649E+00, 1.629E+00, 1.615E+00,  FA18770
     *           1.579E+00, 1.542E+00, 1.508E+00, 1.479E+00, 1.451E+00,  FA18780
     *           1.422E+00, 1.390E+00, 1.356E+00, 1.323E+00, 1.281E+00,  FA18790
     *           1.224E+00, 1.154E+00, 1.066E+00, 9.730E-01, 8.800E-01,  FA18800
     *           7.888E-01, 7.046E-01, 6.315E-01, 5.592E-01, 5.008E-01,  FA18810
     *           4.453E-01, 3.916E-01, 3.389E-01, 2.873E-01, 2.384E-01,  FA18820
     *           1.944E-01, 1.574E-01, 1.500E-01, 1.500E-01, 1.500E-01,  FA18830
     *           1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,  FA18840
     *           1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02,  FA18850
     *           MXZ50*0.0 /                                             FA18860
C                                                                        FA18870
      DATA AMOL36 /                                                      FA18880
     *           1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.697E+00,  FA18890
     *           1.687E+00, 1.672E+00, 1.649E+00, 1.629E+00, 1.615E+00,  FA18900
     *           1.579E+00, 1.542E+00, 1.508E+00, 1.479E+00, 1.451E+00,  FA18910
     *           1.422E+00, 1.390E+00, 1.356E+00, 1.323E+00, 1.281E+00,  FA18920
     *           1.224E+00, 1.154E+00, 1.066E+00, 9.730E-01, 8.800E-01,  FA18930
     *           7.931E-01, 7.130E-01, 6.438E-01, 5.746E-01, 5.050E-01,  FA18940
     *           4.481E-01, 3.931E-01, 3.395E-01, 2.876E-01, 2.386E-01,  FA18950
     *           1.944E-01, 1.574E-01, 1.500E-01, 1.500E-01, 1.500E-01,  FA18960
     *           1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,  FA18970
     *           1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02,  FA18980
     *           MXZ50*0.0 /                                             FA18990
C                                                                        FA19000
      DATA AMOL46 /                                                      FA19010
     *           1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.697E+00,  FA19020
     *           1.687E+00, 1.672E+00, 1.649E+00, 1.629E+00, 1.615E+00,  FA19030
     *           1.579E+00, 1.542E+00, 1.506E+00, 1.471E+00, 1.434E+00,  FA19040
     *           1.389E+00, 1.342E+00, 1.290E+00, 1.230E+00, 1.157E+00,  FA19050
     *           1.072E+00, 9.903E-01, 9.170E-01, 8.574E-01, 8.013E-01,  FA19060
     *           7.477E-01, 6.956E-01, 6.442E-01, 5.888E-01, 5.240E-01,  FA19070
     *           4.506E-01, 3.708E-01, 2.992E-01, 2.445E-01, 2.000E-01,  FA19080
     *           1.660E-01, 1.500E-01, 1.500E-01, 1.500E-01, 1.500E-01,  FA19090
     *           1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,  FA19100
     *           1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02,  FA19110
     *           MXZ50*0.0 /                                             FA19120
C                                                                        FA19130
      DATA AMOL56 /                                                      FA19140
     *           1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.697E+00,  FA19150
     *           1.687E+00, 1.672E+00, 1.649E+00, 1.629E+00, 1.615E+00,  FA19160
     *           1.579E+00, 1.542E+00, 1.506E+00, 1.471E+00, 1.434E+00,  FA19170
     *           1.389E+00, 1.342E+00, 1.290E+00, 1.230E+00, 1.161E+00,  FA19180
     *           1.084E+00, 1.014E+00, 9.561E-01, 9.009E-01, 8.479E-01,  FA19190
     *           7.961E-01, 7.449E-01, 6.941E-01, 6.434E-01, 5.883E-01,  FA19200
     *           5.238E-01, 4.505E-01, 3.708E-01, 3.004E-01, 2.453E-01,  FA19210
     *           1.980E-01, 1.590E-01, 1.500E-01, 1.500E-01, 1.500E-01,  FA19220
     *           1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,  FA19230
     *           1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02,  FA19240
     *           MXZ50*0.0 /                                             FA19250
C                                                                        FA19260
      DATA AMOL66 /                                                      FA19270
     *           1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00,  FA19280
     *           1.700E+00, 1.700E+00, 1.699E+00, 1.697E+00, 1.693E+00,  FA19290
     *           1.685E+00, 1.675E+00, 1.662E+00, 1.645E+00, 1.626E+00,  FA19300
     *           1.605E+00, 1.582E+00, 1.553E+00, 1.521E+00, 1.480E+00,  FA19310
     *           1.424E+00, 1.355E+00, 1.272E+00, 1.191E+00, 1.118E+00,  FA19320
     *           1.055E+00, 9.870E-01, 9.136E-01, 8.300E-01, 7.460E-01,  FA19330
     *           6.618E-01, 5.638E-01, 4.614E-01, 3.631E-01, 2.773E-01,  FA19340
     *           2.100E-01, 1.650E-01, 1.500E-01, 1.500E-01, 1.500E-01,  FA19350
     *           1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,  FA19360
     *           1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02,  FA19370
     *           MXZ50*0.0 /                                             FA19380
C                                                                        FA19390
C     DATA O2 /                                                          FA19400
C                                                                        FA19410
      DATA AMOL17 /                                                      FA19420
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19430
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19440
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19450
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19460
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19470
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19480
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19490
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19500
     *           2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,  FA19510
     *           1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04,  FA19520
     *           MXZ50*0.0 /                                             FA19530
C                                                                        FA19540
      DATA AMOL27 /                                                      FA19550
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19560
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19570
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19580
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19590
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19600
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19610
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19620
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19630
     *           2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,  FA19640
     *           1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04,  FA19650
     *           MXZ50*0.0 /                                             FA19660
C                                                                        FA19670
      DATA AMOL37 /                                                      FA19680
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19690
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19700
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19710
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19720
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19730
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19740
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19750
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19760
     *           2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,  FA19770
     *           1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04,  FA19780
     *           MXZ50*0.0 /                                             FA19790
C                                                                        FA19800
      DATA AMOL47 /                                                      FA19810
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19820
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19830
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19840
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19850
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19860
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19870
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19880
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19890
     *           2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,  FA19900
     *           1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04,  FA19910
     *           MXZ50*0.0 /                                             FA19920
C                                                                        FA19930
      DATA AMOL57 /                                                      FA19940
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19950
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19960
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19970
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19980
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA19990
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20000
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20010
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20020
     *           2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,  FA20030
     *           1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04,  FA20040
     *           MXZ50*0.0 /                                             FA20050
C                                                                        FA20060
      DATA AMOL67 /                                                      FA20070
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20080
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20090
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20100
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20110
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20120
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20130
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20140
     *           2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,  FA20150
     *           2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,  FA20160
     *           1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04,  FA20170
     *           MXZ50*0.0 /                                             FA20180
C                                                                        FA20190
C     DATA DENSITY /                                                     FA20200
C                                                                        FA20210
      DATA AMOL18 /                                                      FA20220
     *           2.450E+19, 2.231E+19, 2.028E+19, 1.827E+19, 1.656E+19,  FA20230
     *           1.499E+19, 1.353E+19, 1.218E+19, 1.095E+19, 9.789E+18,  FA20240
     *           8.747E+18, 7.780E+18, 6.904E+18, 6.079E+18, 5.377E+18,  FA20250
     *           4.697E+18, 4.084E+18, 3.486E+18, 2.877E+18, 2.381E+18,  FA20260
     *           1.981E+18, 1.651E+18, 1.381E+18, 1.169E+18, 9.920E+17,  FA20270
     *           8.413E+17, 5.629E+17, 3.807E+17, 2.598E+17, 1.789E+17,  FA20280
     *           1.243E+17, 8.703E+16, 6.147E+16, 4.352E+16, 3.119E+16,  FA20290
     *           2.291E+16, 1.255E+16, 6.844E+15, 3.716E+15, 1.920E+15,  FA20300
     *           9.338E+14, 4.314E+14, 1.801E+14, 7.043E+13, 2.706E+13,  FA20310
     *           1.098E+13, 4.445E+12, 1.941E+12, 8.706E+11, 4.225E+11,  FA20320
     *           MXZ50*0.0 /                                             FA20330
C                                                                        FA20340
      DATA AMOL28 /                                                      FA20350
     *           2.496E+19, 2.257E+19, 2.038E+19, 1.843E+19, 1.666E+19,  FA20360
     *           1.503E+19, 1.351E+19, 1.212E+19, 1.086E+19, 9.716E+18,  FA20370
     *           8.656E+18, 7.698E+18, 6.814E+18, 6.012E+18, 5.141E+18,  FA20380
     *           4.368E+18, 3.730E+18, 3.192E+18, 2.715E+18, 2.312E+18,  FA20390
     *           1.967E+18, 1.677E+18, 1.429E+18, 1.223E+18, 1.042E+18,  FA20400
     *           8.919E+17, 6.050E+17, 4.094E+17, 2.820E+17, 1.927E+17,  FA20410
     *           1.338E+17, 9.373E+16, 6.624E+16, 4.726E+16, 3.398E+16,  FA20420
     *           2.500E+16, 1.386E+16, 7.668E+15, 4.196E+15, 2.227E+15,  FA20430
     *           1.109E+15, 4.996E+14, 1.967E+14, 7.204E+13, 2.541E+13,  FA20440
     *           9.816E+12, 3.816E+12, 1.688E+12, 8.145E+11, 4.330E+11,  FA20450
     *           MXZ50*0.0 /                                             FA20460
C                                                                        FA20470
      DATA AMOL38 /                                                      FA20480
     *           2.711E+19, 2.420E+19, 2.158E+19, 1.922E+19, 1.724E+19,  FA20490
     *           1.542E+19, 1.376E+19, 1.225E+19, 1.086E+19, 9.612E+18,  FA20500
     *           8.472E+18, 7.271E+18, 6.237E+18, 5.351E+18, 4.588E+18,  FA20510
     *           3.931E+18, 3.368E+18, 2.886E+18, 2.473E+18, 2.115E+18,  FA20520
     *           1.809E+18, 1.543E+18, 1.317E+18, 1.125E+18, 9.633E+17,  FA20530
     *           8.218E+17, 5.536E+17, 3.701E+17, 2.486E+17, 1.647E+17,  FA20540
     *           1.108E+17, 7.540E+16, 5.202E+16, 3.617E+16, 2.570E+16,  FA20550
     *           1.863E+16, 1.007E+16, 5.433E+15, 2.858E+15, 1.477E+15,  FA20560
     *           7.301E+14, 3.553E+14, 1.654E+14, 7.194E+13, 3.052E+13,  FA20570
     *           1.351E+13, 6.114E+12, 2.952E+12, 1.479E+12, 7.836E+11,  FA20580
     *           MXZ50*0.0 /                                             FA20590
C                                                                        FA20600
      DATA AMOL48 /                                                      FA20610
     *           2.549E+19, 2.305E+19, 2.080E+19, 1.873E+19, 1.682E+19,  FA20620
     *           1.508E+19, 1.357E+19, 1.216E+19, 1.088E+19, 9.701E+18,  FA20630
     *           8.616E+18, 7.402E+18, 6.363E+18, 5.471E+18, 4.699E+18,  FA20640
     *           4.055E+18, 3.476E+18, 2.987E+18, 2.568E+18, 2.208E+18,  FA20650
     *           1.899E+18, 1.632E+18, 1.403E+18, 1.207E+18, 1.033E+18,  FA20660
     *           8.834E+17, 6.034E+17, 4.131E+17, 2.839E+17, 1.938E+17,  FA20670
     *           1.344E+17, 9.402E+16, 6.670E+16, 4.821E+16, 3.516E+16,  FA20680
     *           2.581E+16, 1.421E+16, 7.946E+15, 4.445E+15, 2.376E+15,  FA20690
     *           1.198E+15, 5.311E+14, 2.022E+14, 7.221E+13, 2.484E+13,  FA20700
     *           9.441E+12, 3.624E+12, 1.610E+12, 7.951E+11, 4.311E+11,  FA20710
     *           MXZ50*0.0 /                                             FA20720
C                                                                        FA20730
      DATA AMOL58 /                                                      FA20740
     *           2.855E+19, 2.484E+19, 2.202E+19, 1.950E+19, 1.736E+19,  FA20750
     *           1.552E+19, 1.383E+19, 1.229E+19, 1.087E+19, 9.440E+18,  FA20760
     *           8.069E+18, 6.898E+18, 5.893E+18, 5.039E+18, 4.308E+18,  FA20770
     *           3.681E+18, 3.156E+18, 2.704E+18, 2.316E+18, 1.982E+18,  FA20780
     *           1.697E+18, 1.451E+18, 1.241E+18, 1.061E+18, 9.065E+17,  FA20790
     *           7.742E+17, 5.134E+17, 3.423E+17, 2.292E+17, 1.533E+17,  FA20800
     *           1.025E+17, 6.927E+16, 4.726E+16, 3.266E+16, 2.261E+16,  FA20810
     *           1.599E+16, 8.364E+15, 4.478E+15, 2.305E+15, 1.181E+15,  FA20820
     *           6.176E+14, 3.127E+14, 1.531E+14, 7.244E+13, 3.116E+13,  FA20830
     *           1.403E+13, 6.412E+12, 3.099E+12, 1.507E+12, 7.814E+11,  FA20840
     *           MXZ50*0.0 /                                             FA20850
C                                                                        FA20860
      DATA AMOL68 /                                                      FA20870
     *           2.548E+19, 2.313E+19, 2.094E+19, 1.891E+19, 1.704E+19,  FA20880
     *           1.532E+19, 1.373E+19, 1.228E+19, 1.094E+19, 9.719E+18,  FA20890
     *           8.602E+18, 7.589E+18, 6.489E+18, 5.546E+18, 4.739E+18,  FA20900
     *           4.050E+18, 3.462E+18, 2.960E+18, 2.530E+18, 2.163E+18,  FA20910
     *           1.849E+18, 1.575E+18, 1.342E+18, 1.144E+18, 9.765E+17,  FA20920
     *           8.337E+17, 5.640E+17, 3.830E+17, 2.524E+17, 1.761E+17,  FA20930
     *           1.238E+17, 8.310E+16, 5.803E+16, 4.090E+16, 2.920E+16,  FA20940
     *           2.136E+16, 1.181E+16, 6.426E+15, 3.386E+15, 1.723E+15,  FA20950
     *           8.347E+14, 3.832E+14, 1.711E+14, 7.136E+13, 2.924E+13,  FA20960
     *           1.189E+13, 5.033E+12, 2.144E+12, 9.688E+11, 5.114E+11,  FA20970
     *           MXZ50*0.0 /                                             FA20980
C                                                                        FA20990
      DATA ANO /  3.00E-04,  3.00E-04,  3.00E-04,  3.00E-04,  3.00E-04,  FA21000
     *            3.00E-04,  3.00E-04,  3.00E-04,  3.00E-04,  3.00E-04,  FA21010
     *            3.00E-04,  3.00E-04,  3.00E-04,  2.99E-04,  2.95E-04,  FA21020
     *            2.83E-04,  2.68E-04,  2.52E-04,  2.40E-04,  2.44E-04,  FA21030
     *            2.55E-04,  2.77E-04,  3.07E-04,  3.60E-04,  4.51E-04,  FA21040
     *            6.85E-04,  1.28E-03,  2.45E-03,  4.53E-03,  7.14E-03,  FA21050
     *            9.34E-03,  1.12E-02,  1.19E-02,  1.17E-02,  1.10E-02,  FA21060
     *            1.03E-02,  1.01E-02,  1.01E-02,  1.03E-02,  1.15E-02,  FA21070
     *            1.61E-02,  2.68E-02,  7.01E-02,  2.13E-01,  7.12E-01,  FA21080
     *            2.08E+00,  4.50E+00,  7.98E+00,  1.00E+01,  1.00E+01,  FA21090
     *            MXZ50*0.0 /                                            FA21100
C                                                                        FA21110
      DATA SO2 /  3.00E-04,  2.74E-04,  2.36E-04,  1.90E-04,  1.46E-04,  FA21120
     *            1.18E-04,  9.71E-05,  8.30E-05,  7.21E-05,  6.56E-05,  FA21130
     *            6.08E-05,  5.79E-05,  5.60E-05,  5.59E-05,  5.64E-05,  FA21140
     *            5.75E-05,  5.75E-05,  5.37E-05,  4.78E-05,  3.97E-05,  FA21150
     *            3.19E-05,  2.67E-05,  2.28E-05,  2.07E-05,  1.90E-05,  FA21160
     *            1.75E-05,  1.54E-05,  1.34E-05,  1.21E-05,  1.16E-05,  FA21170
     *            1.21E-05,  1.36E-05,  1.65E-05,  2.10E-05,  2.77E-05,  FA21180
     *            3.56E-05,  4.59E-05,  5.15E-05,  5.11E-05,  4.32E-05,  FA21190
     *            2.83E-05,  1.33E-05,  5.56E-06,  2.24E-06,  8.96E-07,  FA21200
     *            3.58E-07,  1.43E-07,  5.73E-08,  2.29E-08,  9.17E-09,  FA21210
     *            MXZ50*0.0 /                                            FA21220
C                                                                        FA21230
      DATA ANO2 / 2.30E-05,  2.30E-05,  2.30E-05,  2.30E-05,  2.30E-05,  FA21240
     *            2.30E-05,  2.30E-05,  2.30E-05,  2.30E-05,  2.32E-05,  FA21250
     *            2.38E-05,  2.62E-05,  3.15E-05,  4.45E-05,  7.48E-05,  FA21260
     *            1.71E-04,  3.19E-04,  5.19E-04,  7.71E-04,  1.06E-03,  FA21270
     *            1.39E-03,  1.76E-03,  2.16E-03,  2.58E-03,  3.06E-03,  FA21280
     *            3.74E-03,  4.81E-03,  6.16E-03,  7.21E-03,  7.28E-03,  FA21290
     *            6.26E-03,  4.03E-03,  2.17E-03,  1.15E-03,  6.66E-04,  FA21300
     *            4.43E-04,  3.39E-04,  2.85E-04,  2.53E-04,  2.31E-04,  FA21310
     *            2.15E-04,  2.02E-04,  1.92E-04,  1.83E-04,  1.76E-04,  FA21320
     *            1.70E-04,  1.64E-04,  1.59E-04,  1.55E-04,  1.51E-04,  FA21330
     *            MXZ50*0.0 /                                            FA21340
C                                                                        FA21350
      DATA ANH3 / 5.00E-04,  5.00E-04,  4.63E-04,  3.80E-04,  2.88E-04,  FA21360
     *            2.04E-04,  1.46E-04,  9.88E-05,  6.48E-05,  3.77E-05,  FA21370
     *            2.03E-05,  1.09E-05,  6.30E-06,  3.12E-06,  1.11E-06,  FA21380
     *            4.47E-07,  2.11E-07,  1.10E-07,  6.70E-08,  3.97E-08,  FA21390
     *            2.41E-08,  1.92E-08,  1.72E-08,  1.59E-08,  1.44E-08,  FA21400
     *            1.23E-08,  9.37E-09,  6.35E-09,  3.68E-09,  1.82E-09,  FA21410
     *            9.26E-10,  2.94E-10,  8.72E-11,  2.98E-11,  1.30E-11,  FA21420
     *            7.13E-12,  4.80E-12,  3.66E-12,  3.00E-12,  2.57E-12,  FA21430
     *            2.27E-12,  2.04E-12,  1.85E-12,  1.71E-12,  1.59E-12,  FA21440
     *            1.48E-12,  1.40E-12,  1.32E-12,  1.25E-12,  1.19E-12,  FA21450
     *            MXZ50*0.0 /                                            FA21460
C                                                                        FA21470
      DATA HNO3 / 5.00E-05,  5.96E-05,  6.93E-05,  7.91E-05,  8.87E-05,  FA21480
     *            9.75E-05,  1.11E-04,  1.26E-04,  1.39E-04,  1.53E-04,  FA21490
     *            1.74E-04,  2.02E-04,  2.41E-04,  2.76E-04,  3.33E-04,  FA21500
     *            4.52E-04,  7.37E-04,  1.31E-03,  2.11E-03,  3.17E-03,  FA21510
     *            4.20E-03,  4.94E-03,  5.46E-03,  5.74E-03,  5.84E-03,  FA21520
     *            5.61E-03,  4.82E-03,  3.74E-03,  2.59E-03,  1.64E-03,  FA21530
     *            9.68E-04,  5.33E-04,  2.52E-04,  1.21E-04,  7.70E-05,  FA21540
     *            5.55E-05,  4.45E-05,  3.84E-05,  3.49E-05,  3.27E-05,  FA21550
     *            3.12E-05,  3.01E-05,  2.92E-05,  2.84E-05,  2.78E-05,  FA21560
     *            2.73E-05,  2.68E-05,  2.64E-05,  2.60E-05,  2.57E-05,  FA21570
     *            MXZ50*0.0 /                                            FA21580
C                                                                        FA21590
      DATA OH /   4.40E-08,  4.40E-08,  4.40E-08,  4.40E-08,  4.40E-08,  FA21600
     *            4.40E-08,  4.40E-08,  4.41E-08,  4.45E-08,  4.56E-08,  FA21610
     *            4.68E-08,  4.80E-08,  4.94E-08,  5.19E-08,  5.65E-08,  FA21620
     *            6.75E-08,  8.25E-08,  1.04E-07,  1.30E-07,  1.64E-07,  FA21630
     *            2.16E-07,  3.40E-07,  5.09E-07,  7.59E-07,  1.16E-06,  FA21640
     *            2.18E-06,  5.00E-06,  1.17E-05,  3.40E-05,  8.35E-05,  FA21650
     *            1.70E-04,  2.85E-04,  4.06E-04,  5.11E-04,  5.79E-04,  FA21660
     *            6.75E-04,  9.53E-04,  1.76E-03,  3.74E-03,  7.19E-03,  FA21670
     *            1.12E-02,  1.13E-02,  6.10E-03,  1.51E-03,  2.42E-04,  FA21680
     *            4.47E-05,  1.77E-05,  1.19E-05,  1.35E-05,  2.20E-05,  FA21690
     *            MXZ50*0.0 /                                            FA21700
C                                                                        FA21710
      DATA HF /   1.00E-08,  1.00E-08,  1.23E-08,  1.97E-08,  3.18E-08,  FA21720
     *            5.63E-08,  9.18E-08,  1.53E-07,  2.41E-07,  4.04E-07,  FA21730
     *            6.57E-07,  1.20E-06,  1.96E-06,  3.12E-06,  4.62E-06,  FA21740
     *            7.09E-06,  1.05E-05,  1.69E-05,  2.57E-05,  4.02E-05,  FA21750
     *            5.77E-05,  7.77E-05,  9.90E-05,  1.23E-04,  1.50E-04,  FA21760
     *            1.82E-04,  2.30E-04,  2.83E-04,  3.20E-04,  3.48E-04,  FA21770
     *            3.72E-04,  3.95E-04,  4.10E-04,  4.21E-04,  4.24E-04,  FA21780
     *            4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,  FA21790
     *            4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,  FA21800
     *            4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,  FA21810
     *            MXZ50*0.0 /                                            FA21820
C                                                                        FA21830
      DATA HCL /  1.00E-03,  7.49E-04,  5.61E-04,  4.22E-04,  3.19E-04,  FA21840
     *            2.39E-04,  1.79E-04,  1.32E-04,  9.96E-05,  7.48E-05,  FA21850
     *            5.68E-05,  4.59E-05,  4.36E-05,  6.51E-05,  1.01E-04,  FA21860
     *            1.63E-04,  2.37E-04,  3.13E-04,  3.85E-04,  4.42E-04,  FA21870
     *            4.89E-04,  5.22E-04,  5.49E-04,  5.75E-04,  6.04E-04,  FA21880
     *            6.51E-04,  7.51E-04,  9.88E-04,  1.28E-03,  1.57E-03,  FA21890
     *            1.69E-03,  1.74E-03,  1.76E-03,  1.79E-03,  1.80E-03,  FA21900
     *            1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,  FA21910
     *            1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,  FA21920
     *            1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,  FA21930
     *            MXZ50*0.0 /                                            FA21940
C                                                                        FA21950
      DATA HBR /  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  FA21960
     *            1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  FA21970
     *            1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  FA21980
     *            1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  FA21990
     *            1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  FA22000
     *            1.71E-06,  1.76E-06,  1.90E-06,  2.26E-06,  2.82E-06,  FA22010
     *            3.69E-06,  4.91E-06,  6.13E-06,  6.85E-06,  7.08E-06,  FA22020
     *            7.14E-06,  7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06,  FA22030
     *            7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06,  FA22040
     *            7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06,  FA22050
     *            MXZ50*0.0 /                                            FA22060
C                                                                        FA22070
      DATA HI /   3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22080
     *            3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22090
     *            3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22100
     *            3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22110
     *            3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22120
     *            3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22130
     *            3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22140
     *            3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22150
     *            3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22160
     *            3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  FA22170
     *            MXZ50*0.0 /                                            FA22180
C                                                                        FA22190
      DATA CLO /  1.00E-08,  1.00E-08,  1.00E-08,  1.00E-08,  1.00E-08,  FA22200
     *            1.00E-08,  1.00E-08,  1.00E-08,  1.01E-08,  1.05E-08,  FA22210
     *            1.21E-08,  1.87E-08,  3.18E-08,  5.61E-08,  9.99E-08,  FA22220
     *            1.78E-07,  3.16E-07,  5.65E-07,  1.04E-06,  2.04E-06,  FA22230
     *            4.64E-06,  8.15E-06,  1.07E-05,  1.52E-05,  2.24E-05,  FA22240
     *            3.97E-05,  8.48E-05,  1.85E-04,  3.57E-04,  5.08E-04,  FA22250
     *            6.07E-04,  5.95E-04,  4.33E-04,  2.51E-04,  1.56E-04,  FA22260
     *            1.04E-04,  7.69E-05,  6.30E-05,  5.52E-05,  5.04E-05,  FA22270
     *            4.72E-05,  4.49E-05,  4.30E-05,  4.16E-05,  4.03E-05,  FA22280
     *            3.93E-05,  3.83E-05,  3.75E-05,  3.68E-05,  3.61E-05,  FA22290
     *            MXZ50*0.0 /                                            FA22300
C                                                                        FA22310
      DATA OCS /  6.00E-04,  5.90E-04,  5.80E-04,  5.70E-04,  5.62E-04,  FA22320
     *            5.55E-04,  5.48E-04,  5.40E-04,  5.32E-04,  5.25E-04,  FA22330
     *            5.18E-04,  5.09E-04,  4.98E-04,  4.82E-04,  4.60E-04,  FA22340
     *            4.26E-04,  3.88E-04,  3.48E-04,  3.09E-04,  2.74E-04,  FA22350
     *            2.41E-04,  2.14E-04,  1.88E-04,  1.64E-04,  1.37E-04,  FA22360
     *            1.08E-04,  6.70E-05,  2.96E-05,  1.21E-05,  4.31E-06,  FA22370
     *            1.60E-06,  6.71E-07,  4.35E-07,  3.34E-07,  2.80E-07,  FA22380
     *            2.47E-07,  2.28E-07,  2.16E-07,  2.08E-07,  2.03E-07,  FA22390
     *            1.98E-07,  1.95E-07,  1.92E-07,  1.89E-07,  1.87E-07,  FA22400
     *            1.85E-07,  1.83E-07,  1.81E-07,  1.80E-07,  1.78E-07,  FA22410
     *            MXZ50*0.0 /                                            FA22420
C                                                                        FA22430
      DATA H2CO / 2.40E-03,  1.07E-03,  4.04E-04,  2.27E-04,  1.40E-04,  FA22440
     *            1.00E-04,  7.44E-05,  6.04E-05,  5.01E-05,  4.22E-05,  FA22450
     *            3.63E-05,  3.43E-05,  3.39E-05,  3.50E-05,  3.62E-05,  FA22460
     *            3.62E-05,  3.58E-05,  3.50E-05,  3.42E-05,  3.39E-05,  FA22470
     *            3.43E-05,  3.68E-05,  4.03E-05,  4.50E-05,  5.06E-05,  FA22480
     *            5.82E-05,  7.21E-05,  8.73E-05,  1.01E-04,  1.11E-04,  FA22490
     *            1.13E-04,  1.03E-04,  7.95E-05,  4.82E-05,  1.63E-05,  FA22500
     *            5.10E-06,  2.00E-06,  1.05E-06,  6.86E-07,  5.14E-07,  FA22510
     *            4.16E-07,  3.53E-07,  3.09E-07,  2.76E-07,  2.50E-07,  FA22520
     *            2.30E-07,  2.13E-07,  1.98E-07,  1.86E-07,  1.75E-07,  FA22530
     *            MXZ50*0.0 /                                            FA22540
C                                                                        FA22550
      DATA HOCL / 7.70E-06,  1.06E-05,  1.22E-05,  1.14E-05,  9.80E-06,  FA22560
     *            8.01E-06,  6.42E-06,  5.42E-06,  4.70E-06,  4.41E-06,  FA22570
     *            4.34E-06,  4.65E-06,  5.01E-06,  5.22E-06,  5.60E-06,  FA22580
     *            6.86E-06,  8.77E-06,  1.20E-05,  1.63E-05,  2.26E-05,  FA22590
     *            3.07E-05,  4.29E-05,  5.76E-05,  7.65E-05,  9.92E-05,  FA22600
     *            1.31E-04,  1.84E-04,  2.45E-04,  2.96E-04,  3.21E-04,  FA22610
     *            3.04E-04,  2.48E-04,  1.64E-04,  9.74E-05,  4.92E-05,  FA22620
     *            2.53E-05,  1.50E-05,  1.05E-05,  8.34E-06,  7.11E-06,  FA22630
     *            6.33E-06,  5.78E-06,  5.37E-06,  5.05E-06,  4.78E-06,  FA22640
     *            4.56E-06,  4.37E-06,  4.21E-06,  4.06E-06,  3.93E-06,  FA22650
     *            MXZ50*0.0 /                                            FA22660
C                                                                        FA22670
      DATA AN2 /  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  FA22680
     *            7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  FA22690
     *            7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  FA22700
     *            7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  FA22710
     *            7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  FA22720
     *            7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  FA22730
     *            7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  FA22740
     *            7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  FA22750
     *            7.81E+05,  7.81E+05,  7.81E+05,  7.80E+05,  7.79E+05,  FA22760
     *            7.77E+05,  7.74E+05,  7.70E+05,  7.65E+05,  7.60E+05,  FA22770
     *            MXZ50*0.0 /                                            FA22780
C                                                                        FA22790
      DATA HCN /  1.70E-04,  1.65E-04,  1.63E-04,  1.61E-04,  1.60E-04,  FA22800
     *            1.60E-04,  1.60E-04,  1.60E-04,  1.60E-04,  1.60E-04,  FA22810
     *            1.60E-04,  1.60E-04,  1.60E-04,  1.59E-04,  1.57E-04,  FA22820
     *            1.55E-04,  1.52E-04,  1.49E-04,  1.45E-04,  1.41E-04,  FA22830
     *            1.37E-04,  1.34E-04,  1.30E-04,  1.25E-04,  1.19E-04,  FA22840
     *            1.13E-04,  1.05E-04,  9.73E-05,  9.04E-05,  8.46E-05,  FA22850
     *            8.02E-05,  7.63E-05,  7.30E-05,  7.00E-05,  6.70E-05,  FA22860
     *            6.43E-05,  6.21E-05,  6.02E-05,  5.88E-05,  5.75E-05,  FA22870
     *            5.62E-05,  5.50E-05,  5.37E-05,  5.25E-05,  5.12E-05,  FA22880
     *            5.00E-05,  4.87E-05,  4.75E-05,  4.62E-05,  4.50E-05,  FA22890
     *            MXZ50*0.0 /                                            FA22900
C                                                                        FA22910
      DATA CH3CL/ 7.00E-04,  6.70E-04,  6.43E-04,  6.22E-04,  6.07E-04,  FA22920
     *            6.02E-04,  6.00E-04,  6.00E-04,  5.98E-04,  5.94E-04,  FA22930
     *            5.88E-04,  5.79E-04,  5.66E-04,  5.48E-04,  5.28E-04,  FA22940
     *            5.03E-04,  4.77E-04,  4.49E-04,  4.21E-04,  3.95E-04,  FA22950
     *            3.69E-04,  3.43E-04,  3.17E-04,  2.86E-04,  2.48E-04,  FA22960
     *            1.91E-04,  1.10E-04,  4.72E-05,  1.79E-05,  7.35E-06,  FA22970
     *            3.03E-06,  1.32E-06,  8.69E-07,  6.68E-07,  5.60E-07,  FA22980
     *            4.94E-07,  4.56E-07,  4.32E-07,  4.17E-07,  4.05E-07,  FA22990
     *            3.96E-07,  3.89E-07,  3.83E-07,  3.78E-07,  3.73E-07,  FA23000
     *            3.69E-07,  3.66E-07,  3.62E-07,  3.59E-07,  3.56E-07,  FA23010
     *            MXZ50*0.0 /                                            FA23020
C                                                                        FA23030
      DATA H2O2 / 2.00E-04,  1.95E-04,  1.92E-04,  1.89E-04,  1.84E-04,  FA23040
     *            1.77E-04,  1.66E-04,  1.49E-04,  1.23E-04,  9.09E-05,  FA23050
     *            5.79E-05,  3.43E-05,  1.95E-05,  1.08E-05,  6.59E-06,  FA23060
     *            4.20E-06,  2.94E-06,  2.30E-06,  2.24E-06,  2.68E-06,  FA23070
     *            3.68E-06,  5.62E-06,  1.03E-05,  1.97E-05,  3.70E-05,  FA23080
     *            6.20E-05,  1.03E-04,  1.36E-04,  1.36E-04,  1.13E-04,  FA23090
     *            8.51E-05,  6.37E-05,  5.17E-05,  4.44E-05,  3.80E-05,  FA23100
     *            3.48E-05,  3.62E-05,  5.25E-05,  1.26E-04,  3.77E-04,  FA23110
     *            1.12E-03,  2.00E-03,  1.68E-03,  4.31E-04,  4.98E-05,  FA23120
     *            6.76E-06,  8.38E-07,  9.56E-08,  1.00E-08,  1.00E-09,  FA23130
     *            MXZ50*0.0 /                                            FA23140
C                                                                        FA23150
      DATA C2H2 / 3.00E-04,  1.72E-04,  9.57E-05,  6.74E-05,  5.07E-05,  FA23160
     *            3.99E-05,  3.19E-05,  2.80E-05,  2.55E-05,  2.40E-05,  FA23170
     *            2.27E-05,  2.08E-05,  1.76E-05,  1.23E-05,  7.32E-06,  FA23180
     *            4.52E-06,  2.59E-06,  1.55E-06,  8.63E-07,  5.30E-07,  FA23190
     *            3.10E-07,  1.89E-07,  1.04E-07,  5.75E-08,  2.23E-08,  FA23200
     *            8.51E-09,  4.09E-09,  2.52E-09,  1.86E-09,  1.52E-09,  FA23210
     *            1.32E-09,  1.18E-09,  1.08E-09,  9.97E-10,  9.34E-10,  FA23220
     *            8.83E-10,  8.43E-10,  8.10E-10,  7.83E-10,  7.60E-10,  FA23230
     *            7.40E-10,  7.23E-10,  7.07E-10,  6.94E-10,  6.81E-10,  FA23240
     *            6.70E-10,  6.59E-10,  6.49E-10,  6.40E-10,  6.32E-10,  FA23250
     *            MXZ50*0.0 /                                            FA23260
C                                                                        FA23270
      DATA C2H6 / 2.00E-03,  2.00E-03,  2.00E-03,  2.00E-03,  1.98E-03,  FA23280
     *            1.95E-03,  1.90E-03,  1.85E-03,  1.79E-03,  1.72E-03,  FA23290
     *            1.58E-03,  1.30E-03,  9.86E-04,  7.22E-04,  4.96E-04,  FA23300
     *            3.35E-04,  2.14E-04,  1.49E-04,  1.05E-04,  7.96E-05,  FA23310
     *            6.01E-05,  4.57E-05,  3.40E-05,  2.60E-05,  1.89E-05,  FA23320
     *            1.22E-05,  5.74E-06,  2.14E-06,  8.49E-07,  3.42E-07,  FA23330
     *            1.34E-07,  5.39E-08,  2.25E-08,  1.04E-08,  6.57E-09,  FA23340
     *            4.74E-09,  3.79E-09,  3.28E-09,  2.98E-09,  2.79E-09,  FA23350
     *            2.66E-09,  2.56E-09,  2.49E-09,  2.43E-09,  2.37E-09,  FA23360
     *            2.33E-09,  2.29E-09,  2.25E-09,  2.22E-09,  2.19E-09,  FA23370
     *            MXZ50*0.0 /                                            FA23380
C                                                                        FA23390
      DATA PH3 /  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23400
     *            1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23410
     *            1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23420
     *            1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23430
     *            1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23440
     *            1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23450
     *            1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23460
     *            1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23470
     *            1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23480
     *            1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  FA23490
     *            MXZ50*0.0 /                                            FA23500
C                                                                        FA23510
      END                                                                FA23520
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE MDLATM (ITYPE,MDL,IREAD,HSPACE)                                
C                                                                        FA23540
C     *****************************************************************  FA23550
C     THIS SUBROUTINE LOADS ONE OF THE 6 BUILT IN ATMOSPHERIC PROFILES   FA23560
C     OR CALLS NSMDL TO READ IN A USER SUPPLIED PROFILE.                 FA23570
C     *****************************************************************  FA23580
C                                                                        FA23590
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA23600
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA23610
C                                                                        FA23620
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA23630
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA23640
     *              NLTEFL,LNFIL4,LNGTH4                                 FA23650
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA23670
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA23680
      COMMON /MSACCT/ IOD,IDIR,ITOP,ISURF,MSPTS,MSPANL(MXLAY),           FA23690
     *                MXPNL1(MXLAY),MSLAY1,ISFILE,JSFILE,KSFILE,         FA23700
     *                LSFILE,MSFILE,IEFILE,JEFILE,KEFILE                 FA23710
      COMMON /MSCONS/ AIRMSS(MXLAY),TGRND,SEMIS(3),HMINMS,HMAXMS,        FA23720
     *                MSFLAG,MSWIT,IODFIL,MSTGLE                         FA23730
                                                                                
      COMMON /c_drive/ ref_lat,hobs,co2mx,ibmax_b,immax_b,                      
     *                 lvl_1_2,jchar_st(10,2),wm(mxzmd)                         
c common block for layer-to-level analytical jacobians                          
      common /dlaydlev/ilevdq,imoldq,iupdwn,                                    
     &    dqdL(mxlay,0:mxmol),dqdU(mxlay,0:mxmol)                               
c                                                                               
      character*1 jchar_st                                                      
c                                                                               
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA23740
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA23750
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA23760
C                                                                        FA23770
      CHARACTER*8      HMOD                                             &FA23780
C                                                                        FA23790
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),   FA23810
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),        FA23820
     *       AMTP(MXMOL,MXPDIM)                                          FA23830
C                                                                        FA23840
      COMMON /MLATM/ ALT(MXZMD),PMDL(MXZMD,6),TMDL(MXZMD,6),             FA23850
     *               AMOL(MXZMD,8,6),ZST(MXZMD),PST(MXZMD),TST(MXZMD),   FA23860
     *               AMOLS(MXZMD,MXMOL)                                  FA23870
      COMMON /MLATMC/ ATMNAM(6)                                          FA23880
      CHARACTER*24 ATMNAM                                                FA23890
      COMMON /TRAC/ TRAC(MXZMD,MXTRAC)                                   FA23900
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FA23910
C                                                                        FA23920
C     ZMDL BLANK COMMON ALTITUDES FOR LBLRTM BOUNDRIES                   FA23930
C     ZMAX /PARMTR/ HIGHEST LBLRTM ALT                                   FA23940
C     ZMIN /PARMTR/ LOWEST LBLRTM ALT                                    FA23950
C     ZPTH BLANK COMMON                                                  FA23960
C     ZST /MLATM/ ORIGINAL LBLRTM ALTITUDES                              FA23970
C                                                                        FA23980
      IF (MDL.EQ.0) GO TO 40                                             FA23990
      IF (MDL.GE.1) IMMAX = 50                                           FA24000
c                                                                               
c     set scaling factor, co2rat, to adjust model vmr (ppm) to that specified   
      co2rat = 1.                                                               
      if (co2mx .gt. 0.) co2rat = co2mx/330.                                    
c                                                                               
      DO 30 I = 1, IMMAX                                                 FA24010
         ZMDL(I) = ALT(I)                                                FA24020
         PM(I) = PMDL(I,MDL)                                             FA24030
         TM(I) = TMDL(I,MDL)                                             FA24040
C                                                                               
C        > Calculate water density and subtract from <                          
C        > total density to obtain dry air density  <                           
C                                                                               
         DENM(1,I) = AMOL(I,1,MDL)*AMOL(I,8,MDL)*1.0E-6                         
         DRYAIR(I) = AMOL(I,8,MDL) - DENM(1,I)                                  
         DO 10 K = 1, 7                                                  FA24050
            IF (K.GT.NMOL) GO TO 10                                      FA24060
            DENM(K,I) = AMOL(I,K,MDL)*1.0E-6*DRYAIR(I)                   FA24070
   10    CONTINUE                                                        FA24080
         IF (NMOL.GT.1) DENM(2,I) = DENM(2,I)*CO2RAT                     FA24090
         DENW(I) = DENM(1,I)                                             FA24100
         DO 20 K = 8, 28                                                 FA24110
            IF (K.GT.NMOL) GO TO 30                                      FA24120
            ITR = K-7                                                    FA24130
C                                                                               
C           < TRAC is the trace constituent information, >                      
C           < obtained from LBLLOW                       >                      
C                                                                               
            DENM(K,I) = TRAC(I,ITR)*1.0E-6*DRYAIR(I)                     FA24140
   20    CONTINUE                                                        FA24150
C                                                                        FA24160
   30 CONTINUE                                                           FA24170
C                                                                        FA24180
      READ (ATMNAM(MDL),900) (HMOD(L),L=1,3)                             FA24190
      GO TO 50                                                           FA24200
C                                                                        FA24210
   40 CALL NSMDL (ITYPE,MDL)                                                    
C                                                                        FA24230
      if (imoldq.eq.-99) then                                                   
          if (immax.ne.ibmax) then                                              
              write(ipr,*) 'Error in Atmosphere Specification:'                 
              write(ipr,*) '   Desired levels must match input grid'            
              write(ipr,*) '   for analytic jacobian calculation'               
              stop 'error in level grid:  see TAPE6'                            
          endif                                                                 
      endif                                                                     
                                                                                
   50 ZMIN = ZMDL(1)                                                     FA24240
C                                                                        FA24250
      DO 70 I = 1, IMMAX                                                 FA24260
         ZST(I) = ZMDL(I)                                                FA24270
         PST(I) = PM(I)                                                  FA24280
         TST(I) = TM(I)                                                  FA24290
         IF (HSPACE+0.001.GT.ZMDL(I)) ISPACE = I                         FA24300
         DO 60 M = 1, NMOL                                               FA24310
            AMOLS(I,M) = DENM(M,I)                                       FA24320
   60    CONTINUE                                                        FA24330
   70 CONTINUE                                                           FA24340
C                                                                        FA24350
      IMMAX = ISPACE                                                     FA24360
      ZMAX = ZMDL(IMMAX)                                                 FA24370
C                                                                        FA24380
      IMLOW = IMMAX                                                      FA24390
C                                                                        FA24410
      RETURN                                                             FA24420
C                                                                        FA24430
  900 FORMAT (3A8)                                                       FA24440
C                                                                        FA24450
      END                                                                FA24460
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE NSMDL (ITYPE,MDL)                                              
C                                                                        FA24480
C     *****************************************************************  FA24490
C                                                                        FA24500
C                                                                        FA24510
C     NOTES TO USER:                                                     FA24520
C                                                                        FA24530
C     THIS SUBROUTINE IS FOR READING IN AN ATMOSPHERIC PROFILE           FA24540
C     CORRESPONDING TO MODEL = 0.  THE PROFILE IS READ IN AFTER          FA24550
C     CONTROL CARD 3.4                                                   FA24560
C                                                                        FA24570
C     CARD 3.4    IMMAX,(HMOD(I),I=1,3)                                  FA24580
C                   (I5,3A8)                                             FA24590
C                                                                        FA24600
C             IMMAX  NUMBER OF BOUNDARIES FOR THE PROFILE                FA24610
C                                                                        FA24620
C             HMOD   A 24 CHARACTER HEADER DESCRIBING THE PROFILE        FA24630
C                                                                        FA24640
C     SEE DETAILS IN RDUNIT ON CARDS 3.5 AND 3.6.1 ... 3.6.N             FA24650
C                                                                        FA24660
C     *****************************************************************  FA24670
C                                                                        FA24680
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA24690
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA24700
C                                                                        FA24710
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA24720
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA24730
     *              NLTEFL,LNFIL4,LNGTH4                                 FA24740
                                                                                
      COMMON /c_drive/ ref_lat,hobs,co2mx,ibmax_b,immax_b,                      
     *                 lvl_1_2,jchar_st(10,2),wm(mxzmd)                         
c                                                                               
      character*1 jchar_st                                                      
c                                                                               
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA24760
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA24770
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA24800
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA24810
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA24820
C                                                                        FA24830
      CHARACTER*8      HMOD                                             &FA24840
C                                                                        FA24850
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),   FA24870
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),         FA2488
     *       AMTP(MXMOL,MXPDIM)                                          FA24890
C                                                                        FA24900
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FA24910
C                                                                        FA24920
      CHARACTER*8      HMOLS                                            &FA24930
C                                                                        FA24940
      COMMON /HMOLS/ HMOLS(MXMOL),JUNIT(MXMOL),WMOL(MXMOL),JUNITP,       FA24950
     *              JUNITT                                               FA24960
C                                                                        FA24970
C     ***********************************************************        FA24980
C                                                                        FA24990
C     COMMON BLOCK FOR GENERIC MOLECULAR DATA INPUT                      FA25000
C                                                                        FA25010
C     ************************************************************       FA25020
C                                                                        FA25030
      if (noprnt .ge.0)  WRITE (IPR,900)                                        
      READ (IRD,905) IMMAX_B,HMOD                                               
C                                                                        FA25060
      IMMAX = ABS(IMMAX_B)                                                      
      IMLOW = IMMAX                                                      FA25070
      if (noprnt .ge.0) WRITE (IPR,910) IMMAX,HMOD                              
      IF (IMMAX.GT.IMDIM) GO TO 30                                       FA25090
c                                                                               
      DO 20 IM = 1, IMMAX                                                FA25100
C                                                                        FA25110
C     READ IN GENERIC UNITS FOR USER MODEL                               FA25120
C                                                                        FA25130
         CALL RDUNIT (IMMAX_B,IM,ZMDL(IM),PM(IM),TM(IM),NMOL,NOPRNT)            
C                                                                        FA25150
C     CONVERSION OF GENERIC UNITS TO DENSITIES FOR LBLRTM RUNS           FA25200
C                                                                        FA25210
         CALL CONVRT (PM(IM),TM(IM),JUNIT,WMOL,IM,NMOL,NOPRNT,co2mx)            
C                                                                        FA25240
         DENW(IM) = DENM(1,IM)                                           FA25250
   20 CONTINUE                                                           FA25270
C                                                                               
                                                                                
      IF (IMMAX_B .LT. 0) THEN                                                  
         CALL CMPALT (IMMAX,PM,TM,DENW,                                         
     &        ZMDL(1),REF_LAT,ZMDL)                                             
      ENDIF                                                                     
                                                                                
      DO 25 IM = 2,IMMAX                                                        
         IF (ZMDL(IM) .LE. ZMDL(IM-1)) GO TO 35                                 
 25      CONTINUE                                                               
                                                                                
      RETURN                                                             FA25290
C                                                                        FA25300
   30 CONTINUE                                                           FA25310
      if (noprnt .ge.0) WRITE (IPR,915) IMMAX,IMDIM                             
C                                                                        FA25330
      STOP ' LEVEL ERROR IN NSMDL '                                      FA25340
C                                                                        FA25350
   35 CONTINUE                                                                  
                                                                                
      if (noprnt .ge.0) WRITE (IPR,920) im,im+1,ZMDL(IM),ZMDL(IM+1)             
                                                                                
      STOP 'INPUT ALTITUDES NOT IN ASCENDING ORDER'                             
                                                                                
  900 FORMAT (///,' READING IN USER SUPPLIED MODEL ATMOSPHERE')          FA25360
  905 FORMAT (I5,3A8)                                                           
  910 FORMAT (//,10X,'IMMAX    = ',I5,/,10X,'PROFILE = ',3A8)                   
  915 FORMAT (/,' NUMBER OF PROFILE LEVELS IMMAX = ',I5,                 FA25390
     *        ' EXCEEDS THE MAXIMUM ALLOWED = ',I5)                      FA25400
  920 FORMAT (///,' ERROR: INPUT ALTITUDES FOR LBLRTM LAYERS ',                 
     *        'ARE NOT IN ASCENDING ORDER',//,5X,                               
     *        ' ZMDL AT GRID PT I,I+1 = ',i5,i5,/,(2F10.4))                     
C                                                                        FA25410
      END                                                                FA25420
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE HEADPR (IPR,NOPRNT)                                     FA25430
C                                                                        FA25440
C     SUBROUTINE TO WRITE HEADER INFORMATION FOR MODEL  0                FA25450
C                                                                        FA25460
      CHARACTER*8      HMOLS                                            &FA25470
C                                                                        FA25480
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA25490
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA25500
C                                                                        FA25510
      COMMON /HMOLS/ HMOLS(MXMOL),JUNIT(MXMOL),WMOL(MXMOL),JUNITP,       FA25520
     *               JUNITT                                              FA25530
C                                                                        FA25540
      WRITE (IPR,900)                                                    FA25550
      WRITE (IPR,905)                                                    FA25560
      WRITE (IPR,910) (I,HMOLS(I),I=1,MXMOL)                             FA25570
      WRITE (IPR,915)                                                    FA25580
C                                                                        FA25590
      RETURN                                                             FA25600
C                                                                        FA25610
  900 FORMAT (/,'  THE USER HAS ELECTED TO PROVIDE THE REQUIRED',/,      FA25620
     *        '  MODEL ATMOSPHERE SPECIFICATIONS.',/,/,                  FA25630
     *        '  SEE DOCUMENTATION OR "SUBROUTINE RDUNIT" FOR ',/,       FA25640
     *        '  ADDITIONAL INFORMATION.',//)                            FA25650
  905 FORMAT ('  USER OPTIONS FOR PRESSURE AND TEMPERATURE ',//,         FA25660
     *        '               JCHAR   JUNIT ',//,                        FA25670
     *        '    PRESSURE " ",A      10    PRESSURE IN (MB)',/,        FA25680
     *        '                 B      11       "     "  (ATM)',/,       FA25690
     *        '                 C      12       "     "  (TORR)',/,      FA25700
     *        '                1-6    1-6    DEFAULT TO SPECIFIED',      FA25710
     *        ' MODEL ATMOSPHERE',//,                                    FA25720
     *        '    TEMP     " ",A      10    AMBIENT TEMP IN DEG(K)',/,  FA25730
     *        '                 B      11       "     "   "   " (C)',/,  FA25740
     *        '                1-6    1-6    DEFAULT TO SPECIFIED',      FA25750
     *        ' MODEL ATMOSPHERE',//)                                    FA25760
  910 FORMAT (/,' AVAILABLE     ',7('(',I2,')',A8),/,' MOL. SPECIES',    FA25770
     *        (T16,7('(',I2,')',A8)))                                    FA25780
  915 FORMAT (/,'  POTENTIAL CHOICE OF UNITS FOR ABOVE SPECIES',/,       FA25790
     *        ' JCHAR = " ",A    - VOLUME MIXING RATIO (PPMV)',/,        FA25800
     *        '       = B        - NUMBER DENSITY (CM-3)',/,             FA25810
     *        '       = C        - MASS MIXING RATIO (GM/KG)',/,         FA25820
     *        '       = D        - MASS DENSITY (GM M-3)',/,             FA25830
     *        '       = E        - PARTIAL PRESSURE (MB)',/,             FA25840
     *        '       = F        - DEW POINT TEMP (K) * H2O ONLY *',/,   FA25850
     *        '       = G        - DEW POINT TEMP (C) * H2O ONLY *',/,   FA25860
     *        '       = H        - RELATIVE HUMIDITY (PERCENT) ',        FA25870
     *                             '*H2O ONLY*',/,                       FA25880
     *        '       = I        - AVAILABLE FOR USER DEFINITION',/,     FA25890
     *        '       = 1-6      - DEFAULT TO SPECIFIED MODEL ',         FA25900
     *        'ATMOSPHERE',/,' JCHAR MUST BE LESS THAN "J"',/)           FA25910
C                                                                        FA25920
      END                                                                FA25930
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE RDUNIT (IMMAX_B,IM,ZMDL,PM,TM,NMOL,NOPRNT)                     
C                                                                        FA25950
C     *******************************************************            FA25960
C                                                                        FA25970
C       SUBROUTINE DESIGNED TO READ NEW MOLECULAR DATA INPUT             FA25980
C        PARAMETERS - JCHAR = INPUT KEY (SEE BELOW)                      FA25990
C                     WMOL  = INPUT VALUE FOR LAYER                      FA26000
C                                                                        FA26010
C     ***  ROUTINE ALSO ACCEPTS VARIABLE UNITS ON PRESS AND TEMP         FA26020
C     ***  THE ASSOCIATED 'JUNIT' DEFINITIONS ARE CONTAINED IN           FA26030
C               JUNITP, AND JUNITT                                       FA26040
C          SEE INPUT KEY BELOW                                           FA26050
C                                                                        FA26060
C                                                                        FA26070
C       NMOL = NUMBER OF MOLECULAR SPECIES TO BE CONSIDERED              FA26080
C               (ORDER IS THAT OF AFGL LINE PARAMETER TAPE)              FA26090
C                                                                        FA26100
C     FOR MOLECULAR SPECIES ONLY                                         FA26110
C                                                                        FA26120
C       JCHAR   JUNIT                                                    FA26130
C                                                                        FA26140
C     " ",A      10    VOLUME MIXING RATIO (PPMV)                        FA26150
C         B      11    NUMBER DENSITY (CM-3)                             FA26160
C         C      12    MASS MIXING RATIO (GM(K)/KG(AIR))                 FA26170
C         D      13    MASS DENSITY (GM M-3)                             FA26180
C         E      14    PARTIAL PRESSURE (MB)                             FA26190
C         F      15    DEW POINT TEMP (TD IN T(K)) - H2O ONLY            FA26200
C         G      16     "    "     "  (TD IN T(C)) - H2O ONLY            FA26210
C         H      17    RELATIVE HUMIDITY (RH IN PERCENT) - H2O ONLY      FA26220
C         I      18    AVAILABLE FOR USER DEFINITION                     FA26230
C        1-6    1-6    DEFAULT TO SPECIFIED MODEL ATMOSPHERE             FA26240
C                                                (SEE KEY BELOW)         FA26250
C                                                                        FA26260
C     ****************************************************************   FA26270
C     ****************************************************************   FA26280
C                                                                        FA26290
C     ***** OTHER 'JCHAR' SPECIFICATIONS - JCHARP,JCHART                 FA26300
C                                                                        FA26310
C       JCHAR   JUNIT                                                    FA26320
C                                                                        FA26330
C      " ",A     10    PRESSURE IN (MB)                                  FA26340
C          B     11       "     "  (ATM)                                 FA26350
C          C     12       "     "  (TORR)                                FA26360
C         1-6   1-6    DEFAULT TO SPECIFIED MODEL ATMOSPHERE             FA26370
C                                                                        FA26380
C      " ",A     10    AMBIENT TEMPERATURE IN DEG(K)                     FA26390
C          B     11       "         "       "  " (C)                     FA26400
C          C     12       "         "       "  " (F)                     FA26410
C         1-6   1-6    DEFAULT TO SPECIFIED MODEL ATMOSPHERE             FA26420
C                                                                        FA26430
C     ***** DEFINITION OF "DEFAULT" CHOICES FOR PROFILE SELECTION *****  FA26440
C                                                                        FA26450
C      FOR THE USER WHO WISHES TO ENTER ONLY SELECTED ORIGINAL           FA26460
C      VERTICAL PROFILES AND WANTS STANDARD ATMOSPHERE SPECIFICATIONS    FA26470
C      FOR THE OTHERS, THE FOLLOWING OPTION IS AVAILABLE                 FA26480
C                                                                        FA26490
C     *** JCHAR(P,T OR K) MUST = 1-6 (AS ABOVE)                          FA26500
C                                                                        FA26510
C      FOR MOLECULES 8-35, ONLY US STD PROFILES ARE AVIALABLE            FA26520
C      THEREFORE, WHEN  'JCHAR(K) = 1-5', JCHAR(K) WILL BE RESET TO 6    FA26530
C                                                                        FA26540
C     *************************************************************      FA26550
C     *************************************************************      FA26560
C                                                                        FA26570
C                                                                        FA26580
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA26590
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA26600
C                                                                        FA26610
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA26620
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA26630
     *              NLTEFL,LNFIL4,LNGTH4                                 FA26640
C                                                                        FA26650
      CHARACTER*8      HMOLS                                            &FA26660
C                                                                        FA26670
      CHARACTER*8      HMOD                                             &FA24840
      COMMON /CMN/HMOD(3),                                                      
     *       ZMDL_st(MXZMD),PM_st(MXZMD),TM_st(MXZMD),RFNDXM_st(MXZMD),         
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),   FA24870
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),        FA24880
     *       AMTP(MXMOL,MXPDIM)                                          FA24890
C                                                                        FA24900
      COMMON /HMOLS/ HMOLS(MXMOL),JUNIT(MXMOL),WMOL(MXMOL),JUNITP,       FA26680
     *               JUNITT                                              FA26690
      CHARACTER*1 JCHAR,JCHARP,JCHART,JLONG                              FA26700
c                                                                               
      COMMON /MCHAR/ JCHAR(MXMOL),JCHARP,JCHART,JLONG                    FA26710
                                                                                
      COMMON /c_drive/ ref_lat,hobs,co2mx,ibmax_b,immax_dum,                    
     *                 lvl_1_2,jchar_st(10,2),wm(mxzmd)                         
c                                                                               
      character*1 jchar_st                                                      
c                                                                               
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
C                                                                        FA26740
C     ***********************************************************        FA26750
C                                                                        FA26760
C       COMMON BLOCK FOR GENERIC MOLECULAR DATA INPUT                    FA26770
C                                                                        FA26780
C                                                                        FA26790
C     ************************************************************       FA26800
C                                                                        FA26810
      DIMENSION JOLD(MXMOL),KUNIT(MXMOL)                                 FA26820
C                                                                        FA26830
      DATA JOLD / MXMOL*99 /                                             FA26840
      DATA C1 / 18.9766 /,C2 / -14.9595 /,C3 / -2.4388 /                 FA26850
C                                                                        FA26860
      IF (IM.EQ.0) CALL HEADPR (IPR,NOPRNT)                              FA26870
C                                                                        FA26880
C     *********************************************************          FA26890
C                                                                        FA26900
C     INPUT READ FOR 'MODEL = 0", I.E. USER-SUPPLIED VERITCAL            FA26910
C                                                                        FA26920
C     **********************************************************         FA26930
C                                                                        FA26940
      READ (IRD,900) ZMDL,PM,TM,JCHARP,JCHART,JLONG,                            
     *               (JCHAR(K),K=1,MXMOL)                                       
      ISAME = 0                                                          FA26960
      JUNITP = JOU(JCHARP)                                               FA26970
      JUNITT = JOU(JCHART)                                               FA26980
      DO 10 K = 1, NMOL                                                  FA26990
         JUNIT(K) = JOU(JCHAR(K))                                        FA27000
C                                                                        FA27010
C    TEST TO SEE IF INPUT UNITS HAVE CHANGED FROM PREVIOUS READ          FA27020
C                                                                        FA27030
         IF (JOLD(K).NE.JUNIT(K)) ISAME = 1                              FA27040
         KUNIT(K) = JUNIT(K)+1                                           FA27050
   10 CONTINUE                                                           FA27060
C                                                                        FA27070
C     Read in moleclar information at E15.8 format for flag JLONG='L'           
      IF (JLONG.EQ.'L') THEN                                                    
         READ (IRD,906) (WMOL(K),K=1,NMOL)                                      
      ELSEIF (JLONG.EQ.' ') THEN                                                
         READ (IRD,905) (WMOL(K),K=1,NMOL)                                      
      ELSE                                                                      
         WRITE(*,*) 'INVALID VALUE FOR JLONG ON RECORD 3.5: ',JLONG             
         STOP 'RDUNIT'                                                          
      ENDIF                                                                     
      IF (IM.EQ.0) WRITE (IPR,910)                                       FA27090
C                                                                        FA27100
      IF (JLONG.EQ.'L') THEN                                                    
         WRITE (IPR,916) IM,ZMDL,JCHARP,PM,JCHART,TM,                           
     *                   (K,JCHAR(K),WMOL(K),K=1,NMOL)                          
      ELSE                                                                      
         WRITE (IPR,915) IM,ZMDL,JCHARP,PM,JCHART,TM,                           
     *                   (K,JCHAR(K),WMOL(K),K=1,NMOL)                          
      ENDIF                                                                     
                                                                                
      DO 20 I = 1, NMOL                                                  FA27130
         JOLD(I) = JUNIT(I)                                              FA27140
   20 CONTINUE                                                           FA27150
      CALL CHECK (PM,JUNITP,1)                                           FA27160
      CALL CHECK (TM,JUNITT,2)                                           FA27170
C                                                                        FA27180
C     SUBROUTINE DEFAULT DEFINES WMOL  FOR JCHAR  1-6                    FA27190
C                                                                        FA27200
      IF (IMMAX_B .LT. 0) THEN                                                  
         CALL DEFALT_P (PM,TM)                                                  
      ELSE                                                                      
         CALL DEFALT (ZMDL,PM,TM)                                               
      ENDIF                                                                     
                                                                                
      RETURN                                                             FA27220
C                                                                        FA27230
  900 FORMAT (3E10.3,5X,2A1,1X,A1,1X,38A1)                               FA27240
  905 FORMAT (8E10.3)                                                    FA27250
  906 FORMAT (8E15.8)                                                    FA27250
  910 FORMAT (//,'  ECHO INPUT PARAMETERS FOR USER PROVIDED MODEL',/,    FA27260
     *        '0   (P : UNIT)=   ',5X,'(T : UNIT)=   ',5X,               FA27270
     *        '(MOLECULE NUMBER : UNIT)=   ')                            FA27280
 915  FORMAT ('0',I4,1X,'(ALT:KM)=',F7.3,4X,'(P:',A1,')=',G11.5,4X,             
     *        '(T:',A1,')=',F8.3,/,(5X,7(' (',I2,':',A1,')=',1PE10.3)))         
 916  FORMAT ('0',I4,1X,'(ALT:KM)=',F7.3,4X,'(P:',A1,')=',G11.5,4X,             
     *        '(T:',A1,')=',F8.3,/,(5X,7(' (',I2,':',A1,')=',1PE15.8)))         
C                                                                        FA27310
      END                                                                FA27320
      FUNCTION JOU (CHAR)                                                FA27330
C                                                                        FA27340
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA27350
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA27360
     *              NLTEFL,LNFIL4,LNGTH4                                 FA27370
C                                                                        FA27380
      CHARACTER*1 CHAR,HOLVEC(22)                                        FA27390
      DIMENSION INDX1(22)                                                FA27400
C                                                                        FA27410
      DATA (HOLVEC(I),I=1,22) /                                          FA27420
     *                '1','2','3','4','5','6','0','0','0','0',' ','A',   FA27430
     *                'B','C','D','E','F','G','H','I','J','K'/           FA27440
      DATA (INDX1(I),I=1,22) /                                           FA27450
     *                  1,  2,  3,  4,  5,  6,  0,  0,  0,  0, 10, 10,   FA27460
     *                 11, 12, 13, 14, 15, 16, 17, 18, 19, 20/           FA27470
C                                                                        FA27480
      INDX = 0                                                           FA27490
      DO 10 I = 1, 22                                                    FA27500
         IF (HOLVEC(I).NE.CHAR) GO TO 10                                 FA27510
         INDX = INDX1(I)                                                 FA27520
         GO TO 20                                                        FA27530
   10 CONTINUE                                                           FA27540
   20 IF (INDX.EQ.0) THEN                                                FA27550
         WRITE (IPR,900) CHAR                                            FA27560
         STOP ' JOU: BAD PARAM '                                         FA27570
      ENDIF                                                              FA27580
      JOU = INDX                                                         FA27590
C                                                                        FA27600
      RETURN                                                             FA27610
C                                                                        FA27620
  900 FORMAT ('0 INVALID PARAMETER :',2X,A1)                             FA27630
C                                                                        FA27640
      END                                                                FA27650
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE CHECK (A,IA,KEY)                                        FA27660
C                                                                        FA27670
C      UNITS CONVERSION FOR P AND T                                      FA27680
C                                                                        FA27690
C     A = P OR T     AND  IA =JUNITP(I.E. MB,ATM,TORR)                   FA27700
C                            =JUNITT(I.E. DEG K OR C)                    FA27710
C                            =JUNITR(I.E. KM,M,OR CM)                    FA27720
C                                                                        FA27730
      DATA PMB / 1013.25 /,PTORR / 760. /,DEGK / 273.15 /                FA27740
C                                                                        FA27750
      IF (IA.LE.10) RETURN                                               FA27760
C                                                                        FA27770
      GO TO (10,20,30) KEY                                               FA27780
C                                                                        FA27790
C     PRESSURE CONVERSIONS                                               FA27800
C                                                                        FA27810
   10 IF (IA.EQ.11) THEN                                                 FA27820
         A = A*PMB                                                       FA27830
         RETURN                                                          FA27840
      ELSEIF (IA.EQ.12) THEN                                             FA27850
         A = A*PMB/PTORR                                                 FA27860
         RETURN                                                          FA27870
      ELSE                                                               FA27880
         STOP ' CHECK(P)'                                                FA27890
      ENDIF                                                              FA27900
C                                                                        FA27910
C     TEMPERATURE COMVERSIONS                                            FA27920
C                                                                        FA27930
   20 IF (IA.LE.11) THEN                                                 FA27940
         A = A+DEGK                                                      FA27950
         RETURN                                                          FA27960
      ELSE                                                               FA27970
         STOP ' CHECK(T)'                                                FA27980
      ENDIF                                                              FA27990
C                                                                        FA28000
C      RANGE CONVERSIONS                                                 FA28010
C                                                                        FA28020
   30 IF (IA.EQ.11) THEN                                                 FA28030
         A = A/1.E3                                                      FA28040
         RETURN                                                          FA28050
      ELSEIF (IA.EQ.12) THEN                                             FA28060
         A = A/1.E5                                                      FA28070
         RETURN                                                          FA28080
      ELSE                                                               FA28090
         STOP ' CHECK(R)'                                                FA28100
      ENDIF                                                              FA28110
C                                                                        FA28120
      END                                                                FA28130
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE DEFALT (Z,P,T)                                                 
C                                                                        FA28150
C     *****************************************************************  FA28160
C                                                                        FA28170
C     THIS SUBROUTINE LOADS ONE OF THE 6 BUILT IN ATMOSPHERIC PROFILES   FA28180
C     FROM WHICH IT WILL INTERPOLATE "DEFAULT" VALUES FOR ALTITUDE "Z"   FA28190
C                                                                        FA28200
C                                                                        FA28210
C      ***  THIS SUBROUTINE IS CALLED BY "RDUNIT" WHICH                  FA28220
C      ***  READS USER SUPPLIED INPUT PROFILES OR SINGLE VALUES          FA28230
C      ***  UNDER "MODEL = 0     " SPECIFICATIONS                        FA28240
C                                                                        FA28250
C      *** SEE DOCUMENTATION FOR CLARIFICATION ***                       FA28260
C                                                                        FA28270
C     SUBROUTINE "DEFALT"IS TRIGGERRED WHENEVER ANY ONE OF               FA28280
C     THE INPUT PARAMETERS JCHARP, JCART, (JCHAR(K),K=1,NMOL) IS = 1-6   FA28290
C                                                                        FA28300
C     FOR SIMPLICITY, ALL INTERPOLATIONS ARE DONE AT ONE TIME BECAUSE    FA28310
C     THE LAGRANGE WEIGHTS (4PT), BASED ON (ALT-Z), REMAIN UNCHANGED     FA28320
C                                                                        FA28330
C     JCHARP,JCHART AND JCHAR(K) FOR K<8 ALLOW MODEL-DEPENDENT CHOICES   FA28340
C                                                                        FA28350
C                   JCHAR=JUNIT                                          FA28360
C                                                                        FA28370
C                        1       CHOOSES TROPICAL                        FA28380
C                        2         "     MID-LATITUDE SUMMER             FA28390
C                        3         "     MID-LATITUDE WINTER             FA28400
C                        4         "     HIGH-LAT SUMMER                 FA28410
C                        5         "     HIGH-LAT WINTER                 FA28420
C                        6         "     US STANDARD                     FA28430
C                                                                        FA28440
C                                                                        FA28450
C     JUNIT(K) FOR K>7 CHOOSES FROM THE SINGLE TRACE CONSTITUENT         FA28460
C        PROFILES, ALL APPRORIATE FOR THE US STD ATMOSPHERE              FA28470
C                                                                        FA28480
C     ***  NOTE ***  T<0 WILL ALSO PRINT OUT A MESSAGE INDICATING        FA28490
C     ***  A POSSIBLE MISAPPLICATION OF TEMPERATURE UNITS, (K) VS (C)    FA28500
C                                                                        FA28510
C     *****************************************************************  FA28520
C                                                                        FA28530
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA28540
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA28550
C                                                                        FA28560
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA28570
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA28580
     *              NLTEFL,LNFIL4,LNGTH4                                 FA28590
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA28610
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA28620
C                                                                        FA28630
      CHARACTER*8      HMOLS                                            &FA28640
C                                                                        FA28650
      COMMON /HMOLS/ HMOLS(MXMOL),JUNIT(MXMOL),WMOL(MXMOL),JUNITP,       FA28660
     *               JUNITT                                              FA28670
      COMMON /MLATM/ ALT(MXZMD),PMATM(MXZMD,6),TMATM(MXZMD,6),           FA28680
     *               AMOL(MXZMD,8,6),ZST(MXZMD),PST(MXZMD),TST(MXZMD),   FA28690
     *               AMOLS(MXZMD,MXMOL)                                  FA28700
      COMMON /MLATMC/ ATMNAM(6)                                          FA28710
      CHARACTER*24 ATMNAM                                                FA28720
      COMMON /TRAC/ TRAC(MXZMD,MXTRAC)                                   FA28730
C                                                                        FA28740
C     *** 4PT INTERPOLATION FUNCTION                                     FA28750
C                                                                        FA28760
      VAL(A1,A2,A3,A4,X1,X2,X3,X4) = A1*X1+A2*X2+A3*X3+A4*X4             FA28770
C                                                                        FA28780
      ILOWER = 0                                                         FA28790
      IUPPER = 0                                                         FA28800
      IM50 = 50                                                          FA28810
      DO 10 IM = 2, IM50                                                 FA28820
         I2 = IM                                                         FA28830
         IF (ALT(IM).GE.Z) GO TO 20                                      FA28840
   10 CONTINUE                                                           FA28850
      I2 = IM50                                                          FA28860
   20 I1 = I2-1                                                          FA28870
      I0 = I2-2                                                          FA28880
      I3 = I2+1                                                          FA28890
      IF (I0.LT.1) GO TO 30                                              FA28900
      IF (I3.GT.IM50) GO TO 40                                           FA28910
C                                                                        FA28920
      GO TO 60                                                           FA28930
C                                                                        FA28940
C     LOWER ENDPOINT CORRECTION                                          FA28950
C                                                                        FA28960
   30 CONTINUE                                                           FA28970
      ILOWER = 1                                                         FA28980
      I0 = I1                                                            FA28990
      I1 = I2                                                            FA29000
      I2 = I3                                                            FA29010
      I3 = I3+1                                                          FA29020
      GO TO 60                                                           FA29030
C                                                                        FA29040
C     UPPER ENDPOINT CORRECTION                                          FA29050
C                                                                        FA29060
   40 CONTINUE                                                           FA29070
      IUPPER = 1                                                         FA29080
      IF (Z.GT.ALT(IM50)) GO TO 50                                       FA29090
      I3 = I2                                                            FA29100
      I2 = I1                                                            FA29110
      I1 = I0                                                            FA29120
      I0 = I1-1                                                          FA29130
      GO TO 60                                                           FA29140
C                                                                        FA29150
C      UPPER ENDPOINT EXTRAPOLATION                                      FA29160
C                                                                        FA29170
   50 CONTINUE                                                           FA29180
      Z0 = ALT(I0)                                                       FA29190
      Z1 = ALT(I1)                                                       FA29200
      Z2 = ALT(I2)                                                       FA29210
      Z3 = Z2+2.*(Z-Z2)                                                  FA29220
      IUPPER = 2                                                         FA29230
      WRITE (IPR,900) Z                                                  FA29240
C                                                                        FA29250
      STOP 'DEFAULT Z'                                                   FA29260
C                                                                        FA29270
C     LAGRANGE CONTINUATION                                              FA29280
C                                                                        FA29290
   60 CONTINUE                                                           FA29300
C                                                                        FA29310
C     LAGRANGE COEF DETERMINATION                                        FA29320
C                                                                        FA29330
      Z1 = ALT(I1)                                                       FA29340
      Z2 = ALT(I2)                                                       FA29350
      Z0 = ALT(I0)                                                       FA29360
      Z3 = ALT(I3)                                                       FA29370
      DEN1 = (Z0-Z1)*(Z0-Z2)*(Z0-Z3)                                     FA29380
      DEN2 = (Z1-Z2)*(Z1-Z3)*(Z1-Z0)                                     FA29390
      DEN3 = (Z2-Z3)*(Z2-Z0)*(Z2-Z1)                                     FA29400
      DEN4 = (Z3-Z0)*(Z3-Z1)*(Z3-Z2)                                     FA29410
      A1 = ((Z-Z1)*(Z-Z2)*(Z-Z3))/DEN1                                   FA29420
      A2 = ((Z-Z2)*(Z-Z3)*(Z-Z0))/DEN2                                   FA29430
      A3 = ((Z-Z3)*(Z-Z0)*(Z-Z1))/DEN3                                   FA29440
      A4 = ((Z-Z0)*(Z-Z1)*(Z-Z2))/DEN4                                   FA29450
C                                                                        FA29460
C     TEST INPUT PARAMETERS (JUNIT'S) SEQUENTIALLY FOR TRIGGER           FA29470
C      I.E.  JUNIT(P,T,K) = 1-6                                          FA29480
C                                                                        FA29490
      IF (JUNITP.GT.6) GO TO 70                                          FA29500
      MATM = JUNITP                                                      FA29510
C                                                                        FA29520
C     WRITE (IPR,60) Z,MATM                                              FA29530
C                                                                        FA29540
      X1 =  LOG(PMATM(I0,MATM))                                          FA29550
      X2 =  LOG(PMATM(I1,MATM))                                          FA29560
      X3 =  LOG(PMATM(I2,MATM))                                          FA29570
      X4 =  LOG(PMATM(I3,MATM))                                          FA29580
      IF (IUPPER.EQ.2) X4 = X3+2*(X3-X2)                                 FA29590
      P = VAL(A1,A2,A3,A4,X1,X2,X3,X4)                                   FA29600
      P = EXP(P)                                                         FA29610
   70 IF (JUNITT.GT.6) GO TO 80                                          FA29620
      MATM = JUNITT                                                      FA29630
C                                                                        FA29640
C     WRITE (IPR,65) Z,MATM                                              FA29650
C                                                                        FA29660
      X1 = TMATM(I0,MATM)                                                FA29670
      X2 = TMATM(I1,MATM)                                                FA29680
      X3 = TMATM(I2,MATM)                                                FA29690
      X4 = TMATM(I3,MATM)                                                FA29700
      T = VAL(A1,A2,A3,A4,X1,X2,X3,X4)                                   FA29710
   80 DO 110 K = 1, NMOL                                                 FA29720
         IF (JUNIT(K).GT.6) GO TO 110                                    FA29730
C                                                                        FA29740
         IF (K.GT.7) GO TO 90                                            FA29750
         MATM = JUNIT(K)                                                 FA29760
C                                                                        FA29770
C     WRITE (IPR,70) K,HMOLS(K),Z,MATM                                   FA29780
C                                                                        FA29790
         X1 = AMOL(I0,K,MATM)                                            FA29800
         X2 = AMOL(I1,K,MATM)                                            FA29810
         X3 = AMOL(I2,K,MATM)                                            FA29820
         X4 = AMOL(I3,K,MATM)                                            FA29830
         GO TO 100                                                       FA29840
   90    ITR = K-7                                                       FA29850
         MATM = 6                                                        FA29860
C                                                                        FA29870
C     WRITE (IPR,70) K,HMOLS(K),Z,MATM                                   FA29880
C                                                                        FA29890
         X1 = TRAC(I0,ITR)                                               FA29900
         X2 = TRAC(I1,ITR)                                               FA29910
         X3 = TRAC(I2,ITR)                                               FA29920
         X4 = TRAC(I3,ITR)                                               FA29930
c                                                                               
 100     WMOL(K) = VAL(A1,A2,A3,A4,X1,X2,X3,X4)                          FA29940
         JUNIT(K) = 10                                                   FA29960
 110  CONTINUE                                                           FA29970
C                                                                        FA29980
      RETURN                                                             FA29990
C                                                                        FA30000
  900 FORMAT (/,'   *** Z IS GREATER THAN 120 KM ***, Z = ',F10.3)       FA30010
C                                                                        FA30020
      END                                                                FA30030
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
                                                                                
      SUBROUTINE DEFALT_P (P,T)                                                 
C                                                                        FA28150
C     *****************************************************************  FA28160
C                                                                        FA28170
C     THIS SUBROUTINE LOADS ONE OF THE 6 BUILT IN ATMOSPHERIC PROFILES   FA28180
C     FROM WHICH IT WILL INTERPOLATE "DEFAULT" VALUES FOR PRESSURE "P"   FA28190
C                                                                        FA28200
C                                                                        FA28210
C      ***  THIS SUBROUTINE IS CALLED BY "RDUNIT" WHICH                  FA28220
C      ***  READS USER SUPPLIED INPUT PROFILES OR SINGLE VALUES          FA28230
C      ***  UNDER "MODEL = 0     " SPECIFICATIONS                        FA28240
C                                                                        FA28250
C      *** SEE DOCUMENTATION FOR CLARIFICATION ***                       FA28260
C                                                                        FA28270
C     SUBROUTINE "DEFALT"IS TRIGGERRED WHENEVER ANY ONE OF               FA28280
C     THE INPUT PARAMETERS JCHARP, JCART, (JCHAR(K),K=1,NMOL) IS = 1-6   FA28290
C                                                                        FA28300
C     FOR SIMPLICITY, ALL INTERPOLATIONS ARE DONE AT ONE TIME BECAUSE    FA28310
C     THE LAGRANGE WEIGHTS (4PT), BASED ON (ALT-Z), REMAIN UNCHANGED     FA28320
C                                                                        FA28330
C     JCHARP,JCHART AND JCHAR(K) FOR K<8 ALLOW MODEL-DEPENDENT CHOICES   FA28340
C                                                                        FA28350
C                   JCHAR=JUNIT                                          FA28360
C                                                                        FA28370
C                        1       CHOOSES TROPICAL                        FA28380
C                        2         "     MID-LATITUDE SUMMER             FA28390
C                        3         "     MID-LATITUDE WINTER             FA28400
C                        4         "     HIGH-LAT SUMMER                 FA28410
C                        5         "     HIGH-LAT WINTER                 FA28420
C                        6         "     US STANDARD                     FA28430
C                                                                        FA28440
C                                                                        FA28450
C     JUNIT(K) FOR K>7 CHOOSES FROM THE SINGLE TRACE CONSTITUENT         FA28460
C        PROFILES, ALL APPRORIATE FOR THE US STD ATMOSPHERE              FA28470
C                                                                        FA28480
C     ***  NOTE ***  T<0 WILL ALSO PRINT OUT A MESSAGE INDICATING        FA28490
C     ***  A POSSIBLE MISAPPLICATION OF TEMPERATURE UNITS, (K) VS (C)    FA28500
C                                                                        FA28510
C     *****************************************************************  FA28520
C                                                                        FA28530
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA28540
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA28550
C                                                                        FA28560
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA28570
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA28580
     *              NLTEFL,LNFIL4,LNGTH4                                 FA28590
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA28610
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA28620
C                                                                        FA28630
      CHARACTER*8      HMOLS                                             FA28640
C                                                                        FA28650
      COMMON /HMOLS/ HMOLS(MXMOL),JUNIT(MXMOL),WMOL(MXMOL),JUNITP,       FA28660
     *               JUNITT                                              FA28670
      COMMON /MLATM/ ALT(MXZMD),PMATM(MXZMD,6),TMATM(MXZMD,6),           FA28680
     *               AMOL(MXZMD,8,6),ZST(MXZMD),PST(MXZMD),TST(MXZMD),   FA28690
     *               AMOLS(MXZMD,MXMOL)                                  FA28700
      COMMON /MLATMC/ ATMNAM(6)                                          FA28710
      CHARACTER*24 ATMNAM                                                FA28720
      COMMON /TRAC/ TRAC(MXZMD,MXTRAC)                                   FA28730
C                                                                        FA28740
C     *** 4PT INTERPOLATION FUNCTION                                     FA28750
C                                                                        FA28760
      VAL(A1,A2,A3,A4,X1,X2,X3,X4) = A1*X1+A2*X2+A3*X3+A4*X4             FA28770
C                                                                        FA28780
      XLOG_P =  LOG(P)                                                          
C                                                                               
      DO 200 J_MDL=1,6                                                          
                                                                                
         ILOWER = 0                                                             
         IUPPER = 0                                                      FA28800
         LVL_50 = 50                                                     FA28810
         DO 10 LVL = 2, LVL_50                                           FA28820
            I2 = LVL                                                     FA28830
            IF (P .GE. PMATM(LVL,J_MDL)) GO TO 20                               
 10      CONTINUE                                                        FA28850
         I2 = LVL_50                                                     FA28860
 20      I1 = I2-1                                                       FA28870
         I0 = I2-2                                                       FA28880
         I3 = I2+1                                                       FA28890
         IF (I0.LT.1) GO TO 30                                           FA28900
         IF (I3.GT.LVL_50) GO TO 40                                      FA28910
C                                                                        FA28920
         GO TO 60                                                        FA28930
C                                                                        FA28940
C     LOWER ENDPOINT CORRECTION                                          FA28950
C                                                                        FA28960
 30      CONTINUE                                                        FA28970
         ILOWER = 1                                                      FA28980
         I0 = I1                                                         FA28990
         I1 = I2                                                         FA29000
         I2 = I3                                                         FA29010
         I3 = I3+1                                                       FA29020
         GO TO 60                                                        FA29030
C                                                                        FA29040
C     UPPER ENDPOINT CORRECTION                                          FA29050
C                                                                        FA29060
 40      CONTINUE                                                        FA29070
         IUPPER = 1                                                      FA29080
         IF (P .LE. PMATM(LVL_50,J_MDL)) GO TO 50                               
         I3 = I2                                                         FA29100
         I2 = I1                                                         FA29110
         I1 = I0                                                         FA29120
         I0 = I1-1                                                       FA29130
         GO TO 60                                                        FA29140
C                                                                        FA29150
C      UPPER ENDPOINT EXTRAPOLATION                                      FA29160
C                                                                        FA29170
 50      CONTINUE                                                        FA29180
         P_0 =  LOG(PMATM(I0,J_MDL))                                            
         P_1 =  LOG(PMATM(I1,J_MDL))                                            
         P_2 =  LOG(PMATM(I2,J_MDL))                                            
         P_3 = P_2+2.*(XLOG_P-P_2)                                       FA29220
         IUPPER = 2                                                      FA29230
         WRITE (IPR,900) P                                               FA29240
C                                                                        FA29250
         STOP 'DEFAULT P'                                                FA29260
C                                                                        FA29270
C     LAGRANGE CONTINUATION                                              FA29280
C                                                                        FA29290
 60      CONTINUE                                                        FA29300
C                                                                        FA29310
C     LAGRANGE COEF DETERMINATION                                        FA29320
C                                                                        FA29330
         P_0 =  LOG(PMATM(I0,J_MDL))                                            
         P_1 =  LOG(PMATM(I1,J_MDL))                                            
         P_2 =  LOG(PMATM(I2,J_MDL))                                            
         P_3 =  LOG(PMATM(I3,J_MDL))                                            
         DEN1 = (P_0-P_1)*(P_0-P_2)*(P_0-P_3)                            FA29380
         DEN2 = (P_1-P_2)*(P_1-P_3)*(P_1-P_0)                            FA29390
         DEN3 = (P_2-P_3)*(P_2-P_0)*(P_2-P_1)                            FA29400
         DEN4 = (P_3-P_0)*(P_3-P_1)*(P_3-P_2)                            FA29410
         A1 = ((XLOG_P-P_1)*(XLOG_P-P_2)*(XLOG_P-P_3))/DEN1                     
         A2 = ((XLOG_P-P_2)*(XLOG_P-P_3)*(XLOG_P-P_0))/DEN2                     
         A3 = ((XLOG_P-P_3)*(XLOG_P-P_0)*(XLOG_P-P_1))/DEN3                     
         A4 = ((XLOG_P-P_0)*(XLOG_P-P_1)*(XLOG_P-P_2))/DEN4                     
C                                                                        FA29460
C     TEST INPUT PARAMETERS (JUNIT'S) SEQUENTIALLY FOR TRIGGER           FA29470
C      I.E.  JUNIT(P,T,K) = 1-6                                          FA29480
C                                                                        FA29490
C     FOR THIS VERSION OF THE SUBROUTINE DRIVEN BY PRESSURE P                   
C     JUNITP IS THE MODEL ATMOSPHERES TO BE USED FOR THE ALTITUDE               
C                                                                               
 70      IF (JUNITT.GT.6 .OR. JUNITT.NE.J_MDL) GO TO 80                         
         MATM = JUNITT                                                   FA29630
C                                                                        FA29640
C     WRITE (IPR,65) P_,MATM                                             FA29650
C                                                                        FA29660
         X1 = TMATM(I0,MATM)                                             FA29670
         X2 = TMATM(I1,MATM)                                             FA29680
         X3 = TMATM(I2,MATM)                                             FA29690
         X4 = TMATM(I3,MATM)                                             FA29700
         T = VAL(A1,A2,A3,A4,X1,X2,X3,X4)                                FA29710
                                                                                
 80      DO 110 K = 1, NMOL                                              FA29720
            IF (JUNIT(K).GT.6  .OR. JUNIT(K).NE.J_MDL) GO TO 110                
C                                                                        FA29740
            IF (K.GT.7) GO TO 90                                         FA29750
            MATM = JUNIT(K)                                              FA29760
C                                                                        FA29770
C     WRITE (IPR,70) K,HMOLS(K),P_,MATM                                  FA29780
C                                                                        FA29790
            X1 = AMOL(I0,K,MATM)                                         FA29800
            X2 = AMOL(I1,K,MATM)                                         FA29810
            X3 = AMOL(I2,K,MATM)                                         FA29820
            X4 = AMOL(I3,K,MATM)                                         FA29830
            GO TO 100                                                    FA29840
 90         ITR = K-7                                                    FA29850
            MATM = 6                                                     FA29860
C                                                                        FA29870
C     WRITE (IPR,70) K,HMOLS(K),P_,MATM                                  FA29880
C                                                                        FA29890
            X1 = TRAC(I0,ITR)                                            FA29900
            X2 = TRAC(I1,ITR)                                            FA29910
            X3 = TRAC(I2,ITR)                                            FA29920
            X4 = TRAC(I3,ITR)                                            FA29930
                                                                                
 100        WMOL(K) = VAL(A1,A2,A3,A4,X1,X2,X3,X4)                       FA29940
            JUNIT(K) = 10                                                FA29960
c                                                                               
 110     CONTINUE                                                        FA29970
C                                                                        FA29980
 200  CONTINUE                                                                  
                                                                                
      RETURN                                                             FA29990
C                                                                        FA30000
  900 FORMAT (/,'   *** P IS GREATER THAN P(120 KM)  ***, P = ',                
     *     1PE10.4)                                                             
C                                                                        FA30020
      END                                                                FA30030
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
                                                                                
      SUBROUTINE CONVRT (P,T,JUNIT,WMOL,IM,NMOL,NOPRNT,co2mx)                   
C                                                                        FA30050
C*************************************************************           FA30060
C                                                                        FA30070
C        WRITTEN APR, 1985 TO ACCOMMODATE 'JCHAR' DEFINITIONS FOR        FA30080
C        UNIFORM DATA INPUT -                                            FA30090
C                                                                        FA30100
C      JCHAR    JUNIT                                                    FA30110
C                                                                        FA30120
C    " ",A       10    VOLUME MIXING RATIO (PPMV)                        FA30130
C        B       11    NUMBER DENSITY (CM-3)                             FA30140
C        C       12    MASS MIXING RATIO (GM(K)/KG(AIR))                 FA30150
C        D       13    MASS DENSITY (GM M-3)                             FA30160
C        E       14    PARTIAL PRESSURE (MB)                             FA30170
C        F       15    DEW POINT TEMP (TD IN T(K)) - H2O ONLY            FA30180
C        G       16     "    "     "  (TD IN T(C)) - H2O ONLY            FA30190
C        H       17    RELATIVE HUMIDITY (RH IN PERCENT) - H2O ONLY      FA30200
C        I       18    AVAILABLE FOR USER DEFINITION                     FA30210
C        J       19    REQUEST DEFAULT TO SPECIFIED MODEL ATMOSPHERE     FA30220
C                                                                        FA30230
C***************************************************************         FA30240
C                                                                        FA30250
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA30260
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA30270
C                                                                        FA30280
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA30290
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA30300
     *              NLTEFL,LNFIL4,LNGTH4                                 FA30310
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)         
C                                                                        FA30340
C                                                                               
      INTEGER JUNIT(MXMOL)                                                      
      DIMENSION WMOL(MXMOL)                                                     
      DATA C1 / 18.9766 /,C2 / -14.9595 /,C3 / -2.4388 /                 FA30350
C                                                                        FA30360
      RHOAIR = ALOSMT*(P/PZERO)*(TZERO/T)                                FA30370
      A = TZERO/T                                                        FA30380
C                                                                               
C     Get water vapor density                                            FA30390
C                                                                               
      CALL WATVAP (P,T,JUNIT(1),WMOL(1),DENM(1,IM),NOPRNT)               FA30400
C                                                                               
C     Determine density of dry air                                              
C                                                                               
      DRYAIR(IM) = RHOAIR - DENM(1,IM)                                   FA30410
C                                                                               
C     Loop through other molecules                                              
C                                                                               
      DO 70 K=2,NMOL                                                     FA30420
         B = AVOGAD/AMWT(K)                                              FA30430
         R = AIRMWT/AMWT(K)                                                     
         DENM(K,IM) = 0.0                                                FA30440
                                                                                
         IF (JUNIT(K).GT.10) GO TO 20                                    FA30450
C                                                                        FA30460
C     GIVEN VOL. MIXING RATIO                                            FA30470
C                                                                        FA30480
         DENM(K,IM) = WMOL(K)*DRYAIR(IM)*1.E-6                           FA30490
         GO TO 70                                                        FA30500
 20      IF (JUNIT(K).NE.11) GO TO 30                                    FA30510
C                                                                        FA30520
C     GIVEN NUMBER DENSITY (CM-3)                                        FA30530
C                                                                        FA30540
         DENM(K,IM) = WMOL(K)                                            FA30550
         GO TO 70                                                        FA30560
 30      CONTINUE                                                        FA30570
         IF (JUNIT(K).NE.12) GO TO 40                                    FA30580
C                                                                        FA30590
C     GIVEN MASS MIXING RATIO (GM KG-1)                                  FA30600
C                                                                        FA30610
         DENM(K,IM) = R*WMOL(K)*1.0E-3*DRYAIR(IM)                        FA30620
         GO TO 70                                                        FA30630
 40      CONTINUE                                                        FA30640
         IF (JUNIT(K).NE.13) GO TO 50                                    FA30650
C                                                                        FA30660
C     GIVEN MASS DENSITY (GM M-3)                                        FA30670
C                                                                        FA30680
         DENM(K,IM) = B*WMOL(K)*1.0E-6                                   FA30690
         GO TO 70                                                        FA30700
 50      CONTINUE                                                        FA30710
         IF (JUNIT(K).NE.14) GO TO 60                                    FA30720
C                                                                        FA30730
C     GIVEN PARTIAL PRESSURE (MB)                                        FA30740
C                                                                        FA30750
         DENM(K,IM) = ALOSMT*(WMOL(K)/PZERO)*(TZERO/T)                   FA30760
         GO TO 70                                                        FA30770
 60      CONTINUE                                                        FA30780
C                                                                        FA30790
C     JUNIT(18) available for user definition here                       FA30800
C                                                                               
C                                                                               
         IF (JUNIT(K).GT.14) THEN                                        FA30810
            WRITE (IPR,900) K,JUNIT(K)                                   FA30820
            STOP ' CONVRT '                                              FA30830
         ENDIF                                                           FA30840
C                                                                        FA30850
 70   CONTINUE                                                           FA30860
c                                                                               
c     set the co2 mixing ratio to specified value                               
      if (co2mx .gt. 0.)  denm(2,im) = co2mx*dryair(im)*1.e-06                  
C                                                                        FA30870
  900 FORMAT (/,'   **** ERROR IN CONVRT ****, JUNIT(',I5,') = ',I5)     FA30880
C                                                                        FA30890
      RETURN                                                                    
C                                                                               
      END                                                                FA30900
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE WATVAP (P,T,JUNIT,WMOL,DENNUM,NOPRNT)                   FA30910
C                                                                        FA30920
C**********************************************************************  FA30930
C                                                                        FA30940
C        WRITTEN APR, 1985 TO ACCOMMODATE 'JCHAR' DEFINITIONS FOR        FA30950
C        UNIFORM DATA INPUT -                                            FA30960
C                                                                        FA30970
C     JCHAR    JUNIT                                                     FA30980
C                                                                        FA30990
C    " ",A       10    VOLUME MIXING RATIO (PPMV)                        FA31000
C        B       11    NUMBER DENSITY (CM-3)                             FA31010
C        C       12    MASS MIXING RATIO (GM(K)/KG(AIR))                 FA31020
C        D       13    MASS DENSITY (GM M-3)                             FA31030
C        E       14    PARTIAL PRESSURE (MB)                             FA31040
C        F       15    DEW POINT TEMP (TD IN T(K)) - H2O ONLY            FA31050
C        G       16     "    "     "  (TD IN T(C)) - H2O ONLY            FA31060
C        H       17    RELATIVE HUMIDITY (RH IN PERCENT) - H2O ONLY      FA31070
C        I       18    AVAILABLE FOR USER DEFINITION                     FA31080
C        J       19    REQUEST DEFAULT TO SPECIFIED MODEL ATMOSPHERE     FA31090
C                                                                        FA31100
C     THIS SUBROUTINE COMPUTES THE WATERVAPOR NUMBER DENSITY (MOL CM-3)  FA31110
C     GIVE HUMIDITY  # TD = DEW POINT TEMP(K,C), RH = RELATIVE           FA31120
C     (PERCENT), PPH2O = WATER VAPOR PARTIAL PRESSURE (MB), DENH2O =     FA31130
C     WATER VAPOR MASS DENSITY (GM M-3),AMSMIX = MASS MIXING RATIO       FA31140
C     (GM/KG).                                                           FA31150
C                     THE FUNCTION DENSAT FOR THE SATURATION             FA31160
C     WATER VAPOR DENSITY OVER WATER IS ACCURATE TO BETTER THAN 1        FA31170
C     PERCENT FROM -50 TO +50 DEG C. (SEE THE LOWTRAN3 OR 5 REPORT)      FA31180
C                                                                        FA31190
C       'JUNIT' GOVERNS CHOICE OF UNITS -                                FA31200
C                                                                        FA31210
C**********************************************************************  FA31220
C                                                                        FA31230
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA31240
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA31250
C                                                                        FA31260
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA31270
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA31280
     *              NLTEFL,LNFIL4,LNGTH4                                 FA31290
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
C                                                                        FA31320
      DATA C1 / 18.9766 /,C2 / -14.9595 /,C3 / -2.4388 /                 FA31330
C                                                                        FA31340
      DENSAT(ATEMP) = ATEMP*B*EXP(C1+C2*ATEMP+C3*ATEMP**2)*1.0E-6        FA31350
C                                                                        FA31360
      RHOAIR = ALOSMT*(P/PZERO)*(TZERO/T)                                FA31370
      A = TZERO/T                                                        FA31380
      B = AVOGAD/AMWT(1)                                                 FA31390
      R = AIRMWT/AMWT(1)                                                 FA31400
      IF (JUNIT.NE.10) GO TO 10                                          FA31410
C                                                                        FA31420
C     GIVEN VOL. MIXING RATIO                                            FA31430
                                                                                
C     Convert using density of dry air.                                         
                                                                                
      WMOL = WMOL*1.E-06                                                        
      DENNUM = (WMOL/(1.+WMOL))*RHOAIR                                          
      GO TO 90                                                           FA31460
   10 IF (JUNIT.NE.11) GO TO 20                                          FA31470
C                                                                        FA31480
C     GIVEN NUMBER DENSITY (CM-3)                                        FA31490
C                                                                        FA31500
      DENNUM = WMOL                                                      FA31510
      GO TO 90                                                           FA31520
   20 CONTINUE                                                           FA31530
      IF (JUNIT.NE.12) GO TO 30                                          FA31540
C                                                                        FA31550
C     GIVEN MASS MIXING RATIO (GM KG-1)                                  FA31560
                                                                                
C     Convert using density of dry air.  The following quadratic is             
C                                                                        FA31570
      WMOL = WMOL*R*1.0E-3                                                      
      DENNUM = (WMOL/(1.+WMOL))*RHOAIR                                          
      GO TO 90                                                           FA31590
   30 CONTINUE                                                           FA31600
      IF (JUNIT.NE.13) GO TO 40                                          FA31610
C                                                                        FA31620
C     GIVEN MASS DENSITY (GM M-3)                                        FA31630
C                                                                        FA31640
      DENNUM = B*WMOL*1.0E-6                                             FA31650
      GO TO 90                                                           FA31660
   40 CONTINUE                                                           FA31670
      IF (JUNIT.NE.14) GO TO 50                                          FA31680
C                                                                        FA31690
C     GIVEN WATER VAPOR PARTIAL PRESSURE (MB)                            FA31700
C                                                                        FA31710
      DENNUM = ALOSMT*(WMOL/PZERO)*(TZERO/T)                             FA31720
      GO TO 90                                                           FA31730
   50 CONTINUE                                                           FA31740
      IF (JUNIT.NE.15) GO TO 60                                          FA31750
C                                                                        FA31760
C     GIVEN DEWPOINT (DEG K)                                             FA31770
C                                                                        FA31780
      ATD = TZERO/(WMOL)                                                 FA31790
      DENNUM = DENSAT(ATD)*(WMOL)/T                                      FA31800
      GO TO 90                                                           FA31810
   60 CONTINUE                                                           FA31820
      IF (JUNIT.NE.16) GO TO 70                                          FA31830
C                                                                        FA31840
C     GIVEN DEWPOINT (DEG C)                                             FA31850
C                                                                        FA31860
      ATD = TZERO/(TZERO+WMOL)                                           FA31870
      DENNUM = DENSAT(ATD)*(TZERO+WMOL)/T                                FA31880
      GO TO 90                                                           FA31890
   70 CONTINUE                                                           FA31900
      IF (JUNIT.NE.17) GO TO 80                                          FA31910
C                                                                        FA31920
C     GIVEN RELATIVE HUMIDITY (PERCENT)                                  FA31930
C                                                                        FA31940
      DENNUM = DENSAT(A)*(WMOL/100.0)                                    FA31950
      GO TO 90                                                           FA31960
   80 WRITE (IPR,900) JUNIT                                              FA31970
      STOP 'JUNIT'                                                       FA31980
   90 CONTINUE                                                           FA31990
      DENST = DENSAT(A)                                                  FA32000
      RHP = 100.0*(DENNUM/DENST)                                         FA32010
      IF (NOPRNT .ge. 0) WRITE (IPR,905) RHP                               FA320
      IF (RHP.LE.100.0) GO TO 100                                        FA32030
      if (noprnt .ge. 0) WRITE (IPR,910) RHP                                    
  100 CONTINUE                                                           FA32050
C                                                                        FA32060
      RETURN                                                             FA32070
C                                                                        FA32080
  900 FORMAT (/,'  **** ERROR IN WATVAP ****, JUNIT = ',I5)              FA32090
  905 FORMAT (8X,'RH = ',F6.2)                                           FA32100
  910 FORMAT (/,' ****** WARNING (FROM WATVAP) # RELATIVE HUMIDTY = ',   FA32110
     *        G10.3,' IS GREATER THAN 100 PERCENT')                      FA32120
C                                                                        FA32130
      END                                                                FA32140
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE FSCGEO (H1,H2,ANGLE,RANGE,BETA,ITYPE,LEN,HMIN,PHI,      FA32150
     *                   IERROR,HOBS)                                    FA32160
C                                                                        FA32170
C     -------------------------------------------------------------             
C     This routine was modified for LBLRTM to reflect changes                   
C     implemented in MODTRAN to solve problems with inconsistent                
C     path parameters.                                                          
C     It was also modified to eliminate GOTO statements in order to             
C     make the program easier to understand.                                    
C     These changes were obtained from H. Snell (March, 1996).                  
C     -------------------------------------------------------------             
C                                                                               
C     *****************************************************************  FA32180
C     FSCGEO INTERPRETS THE ALLOWABLE COMBINATIONS OF INPUT PATH         FA32190
C     PARAMETERS INTO THE STANDARD SET H1,H2,ANGLE,PHI,HMIN, AND LEN.    FA32200
C     THE ALLOWABLE COMBINATIONS OF INPUT PARAMETERS ARE- FOR ITYPE = 2  FA32210
C     (SLANT PATH H1 TO H2) A. H1, H2, AND ANGLE, B. H1, ANGLE, AND      FA32220
C     RANGE, C. H1, H2, AND RANGE, D. H1, H2, AND BETA -                 FA32230
C     FOR ITYPE = 3 (SLANT PATH H1 TO SPACE, H2 = ZMAX(=100 KM,M=1 TO 6  FA32240
C     A. H1 AND ANGLE, B. H1 AND HMIN (INPUT AS H2).                     FA32250
C     THE SUBROUTINE ALSO DETECTS BAD INPUT (IMPOSSIBLE GEOMETRY) AND    FA32260
C     ITYPE = 2 CASES WHICH INTERSECT THE EARTH, AND RETURNS THESE       FA32270
C     CASES WITH ERROR FLAGS.                                            FA32280
C     THE SUBROUTINE FNDHMN IS CALLED TO CALCULATE HMIN, THE MINIMUM     FA32290
C     HEIGHT ALONG THE PATH, AND PHI, THE ZENITH ANGLE AT H2, USING THE  FA32300
C     ATMOSPHERIC PROFILE STORED IN /MDATA/                              FA32310
C     *****************************************************************  FA32320
C                                                                        FA32330
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA32340
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA32350
     *              NLTEFL,LNFIL4,LNGTH4                                 FA32360
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA32380
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA32390
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
C                                                                        FA32400
      ITER = 0                                                           FA32410
C                                                                               
C     Check for error                                                           
C                                                                               
      IF ((ITYPE.NE.3).AND.(ITYPE.NE.2)) GOTO 90                                
C                                                                        FA32430
      IF (ITYPE.EQ.3) THEN                                                      
C                                                                               
C     Slant path to space                                                FA32440
C     NOTE: If both HMIN and ANGLE are zero, then ANGLE is               FA32450
C           assumed specified                                            FA32460
C                                                                        FA32470
          IF (H2.EQ.0) THEN                                                     
C                                                                        FA32490
C             Case 3A: H1,SPACE,ANGLE                                    FA32500
C                                                                        FA32510
              WRITE (IPR,900)                                            FA32520
              H2 = ZMAX                                                  FA32530
              CALL FNDHMN (H1,ANGLE,H2,LEN,ITER,HMIN,PHI,IERROR)         FA32540
                                                                                
          ELSE                                                                  
C                                                                        FA32570
C             Case 3B: H1,HMIN,SPACE                                     FA32580
C                                                                        FA32590
              WRITE (IPR,905)                                            FA32600
              HMIN = H2                                                  FA32610
              H2 = ZMAX                                                  FA32620
              IF (H1.LT.HMIN) GO TO 80                                   FA32630
              CALL FNDHMN (HMIN,90.0,H1,LEN,ITER,HMIN,ANGLE,IERROR)      FA32640
              CALL FNDHMN (HMIN,90.0,H2,LEN,ITER,HMIN,PHI,IERROR)        FA32650
              IF (HMIN.LT.H1) LEN = 1                                    FA32660
          ENDIF                                                                 
      ENDIF                                                                     
C                                                                               
      IF (ITYPE.EQ.2) THEN                                                      
C                                                                               
C       Assign the variable ISELCT to the following cases                       
C       (depending on input parameters):                                        
C                                                                               
C       -----------------------------------------------                         
C       H1   H2   ANGLE  RANGE  BETA  =>   CASE  ISELCT                         
C       -----------------------------------------------                         
C       X    X      X                       2A     21                           
C       X           X      X                2B     22                           
C       X    X             X                2C     23                           
C       X    X                   X          2D     24                           
C       -----------------------------------------------                         
C                                                                               
         IF (RANGE.GT.0.0) THEN                                                 
C                                                                               
C           Must be Case 2B or Case 2C                                          
C                                                                               
            IF (H2.GT.0.0) THEN                                                 
C                                                                               
C              Case 2C                                                          
C                                                                               
               ISELCT=23                                                        
            ELSEIF (ANGLE.EQ.0.0) THEN                                          
               WRITE(IPR,1000)                                                  
               WRITE(*,1000)                                                    
               ISELCT=23                                                        
            ELSE                                                                
C                                                                               
C              Case 2B                                                          
C                                                                               
               ISELCT=22                                                        
            ENDIF                                                               
         ELSEIF (BETA.GT.0.0) THEN                                              
C                                                                               
C           Case 2D (beta cannot be zero)                                       
C                                                                               
            ISELCT=24                                                           
         ELSE                                                                   
C                                                                               
C           Case 2A, since RANGE and BETA are both zero                         
C                                                                               
            ISELCT=21                                                           
         ENDIF                                                                  
C                                                                               
         IF (ISELCT.EQ.21) THEN                                                 
C                                                                        FA32710
C           Case 2A: H1, H2, ANGLE                                       FA32720
C                                                                        FA32730
            if (noprnt .ge.0) WRITE (IPR,910)                                   
            IF (H1.GE.H2.AND.ANGLE.LE.90.0) GO TO 110                    FA32750
            IF (H1.EQ.0.0.AND.ANGLE.GT.90.0) GO TO 120                   FA32760
            IF (H2.LT.H1.AND.ANGLE.GT.90.0) WRITE (IPR,915) LEN          FA32770
            H2ST = H2                                                    FA32780
            CALL FNDHMN (H1,ANGLE,H2,LEN,ITER,HMIN,PHI,IERROR)           FA32790
            IF (H2.NE.H2ST) GO TO 120                                    FA32800
         ENDIF                                                                  
C                                                                               
         IF (ISELCT.EQ.22) THEN                                                 
C                                                                        FA32850
C           Case 2B: H1, ANGLE, RANGE                                    FA32860
C           Assume refraction                                            FA32870
C                                                                        FA32880
            if (noprnt .ge.0) WRITE (IPR,920)                                   
            CALL NEWH2(H1,H2,ANGLE,RANGE,BETA,LEN,HMIN,PHI)                     
         ENDIF                                                                  
C                                                                               
         IF (ISELCT.EQ.23) THEN                                                 
C                                                                        FA33090
C           Case 2C: H1, H2, RANGE                                       FA33100
C                                                                        FA33110
            if (noprnt .ge.0) WRITE (IPR,930)                                   
            IF (ABS(H1-H2).GT.RANGE) GO TO 100                           FA33130
            R1 = H1+RE                                                   FA33140
            R2 = H2+RE                                                   FA33150
C                                                                        FA33160
            ZARG2 = (H1**2-H2**2+RANGE**2+2.0*RE*(H1-H2))                       
     *              /(2.0*R1*RANGE)                                      FA33170
            ERARG2 = ABS(ZARG2)-1.0                                      FA33180
            IF ((ERARG2.LE.1.0E-6).AND.(ERARG2.GE.0.0)) THEN             FA33190
               IF (ZARG2.LT.0.0) THEN                                    FA33200
                  ZARG2 = -1.0                                           FA33210
               ELSE                                                      FA33220
                  ZARG2 = 1.0                                            FA33230
               ENDIF                                                     FA33240
            ENDIF                                                        FA33250
            ANGLE = 180.0-ACOS(ZARG2)*DEG                                FA33260
            ZARG3 = (H2**2-H1**2+RANGE**2+2*RE*(H2-H1))/(2.0*R2*RANGE)   FA33270
            ERARG3 = ABS(ZARG3)-1.0                                      FA33280
            IF ((ERARG3.LE.1.0E-6).AND.(ERARG3.GE.0.0)) THEN             FA33290
               IF (ZARG3.LT.0.0) THEN                                    FA33300
                 ZARG3 = -1.0                                            FA33310
               ELSE                                                      FA33320
                 ZARG3 = 1.0                                             FA33330
               ENDIF                                                     FA33340
            ENDIF                                                        FA33350
            PHI = 180.0-ACOS(ZARG3)*DEG                                  FA33360
            BETA = PHI+ANGLE-180.                                        FA33370
C                                                                        FA33380
            IF (RANGE.GT.2.0.AND.BETA.GT.0) THEN                         FA33390
               CALL FDBETA (H1,H2,BETA,ANGLE,PHI,LEN,HMIN,IERROR)        FA33400
            ELSE                                                         FA33410
               LEN = 0                                                   FA33420
               IF (ANGLE.GT.90.0.AND.PHI.GT.90.0) LEN = 1                FA33430
               CALL FNDHMN (H1,ANGLE,H2,LEN,ITER,HMIN,PHI,IERROR)        FA33440
            ENDIF                                                        FA33450
         ENDIF                                                                  
C                                                                               
         IF (ISELCT.EQ.24) THEN                                                 
C                                                                        FA33470
C        Case 2D: H1, H2, BETA                                           FA33480
C                                                                        FA33490
            CALL FDBETA (H1,H2,BETA,ANGLE,PHI,LEN,HMIN,IERROR)           FA33510
         ENDIF                                                                  
      ENDIF                                                                     
C                                                                        FA33530
C     End of allowed cases                                               FA33540
C                                                                        FA33550
C     Test IERROR and recheck LEN                                        FA33580
C                                                                        FA33590
      IF (IERROR.NE.0) RETURN                                            FA33600
      LEN = 0                                                            FA33610
      IF (HMIN.LT.  MIN(H1,H2)) LEN = 1                                  FA33620
C                                                                        FA33630
C     Reduce path endpoints above ZMAX to ZMAX                           FA33640
C                                                                        FA33650
      IF (HMIN.GE.ZMAX) GO TO 130                                        FA33660
      IF (H1.GT.ZMAX.OR.H2.GT.ZMAX) CALL REDUCE (H1,H2,ANGLE,PHI,ITER)   FA33670
C                                                                        FA33680
C     At this point the following parameters are defined-                FA33690
C         H1,H2,ANGLE,PHI,HMIN,LEN                                       FA33700
C                                                                               
C     Calculate sin(PHI) and sin(ANGLE) and output                              
C                                                                        FA33710
      radconv = 2.*pi/360.                                                      
      sinphi = sin(radconv*phi)                                                 
      sinangle = sin(radconv*angle)                                             
      if (noprnt .ge. 0) WRITE (IPR,935)                                        
     *                   H1,H2,ANGLE,sinangle,PHI,sinphi,HMIN,LEN               
                                                                                
C                                                                               
C     Calculate and output geometry from satellite above 120km.                 
C     Subtract from 180 degrees to correctly place angle in the                 
C     3rd quadrant.                                                             
C                                                                               
      if (hobs.gt.0.) then                                                      
         if (h2.gt.h1) then                                                     
            h_toa = h2                                                          
            sintoa = sinphi                                                     
            toa_ang = phi                                                       
         else                                                                   
            h_toa = h1                                                          
            sintoa = sinangle                                                   
            toa_ang = angle                                                     
         endif                                                                  
         sintoa_sat = ((re+h_toa)/(re+hobs))*sintoa                             
         toa_sat = 180. - asin(sintoa_sat)/radconv                              
         sintoa_sat = sin(radconv*toa_sat)                                      
         diffangle = toa_sat - toa_ang                                          
         WRITE (IPR,937) hobs,toa_sat,sintoa_sat,diffangle                      
      endif                                                                     
                                                                                
                                                                                
C                                                                               
      RETURN                                                             FA33730
C                                                                        FA33740
C     Error messages                                                     FA33750
C                                                                        FA33760
   80 CONTINUE                                                           FA33770
      WRITE (IPR,940) H1,HMIN                                            FA33780
      GO TO 140                                                          FA33790
   90 WRITE (IPR,945) ITYPE,ITYPE                                        FA33800
      GO TO 140                                                          FA33810
  100 WRITE (IPR,950) H1,H2,RANGE                                        FA33820
      GO TO 140                                                          FA33830
  110 CONTINUE                                                           FA33840
      WRITE (IPR,955) H1,H2,ANGLE                                        FA33850
      GO TO 140                                                          FA33860
  120 WRITE (IPR,960)                                                    FA33870
      GO TO 140                                                          FA33880
  130 WRITE (IPR,965) ZMAX,H1,H2,HMIN                                    FA33890
  140 IERROR = 1                                                         FA33900
C                                                                        FA33910
      RETURN                                                             FA33920
C                                                                        FA33930
  900 FORMAT (//,' CASE 3A: GIVEN H1,H2=SPACE,ANGLE')                    FA33940
  905 FORMAT (//,' CASE 3B: GIVEN H1, HMIN, H2=SPACE')                   FA33950
  910 FORMAT (//,' CASE 2A: GIVEN H1, H2, ANGLE')                        FA33960
  915 FORMAT (//,' EITHER A SHORT PATH (LEN=0) OR A LONG PATH ',         FA33970
     *        'THROUGH A TANGENT HEIGHT (LEN=1) IS POSSIBLE: LEN = ',    FA33980
     *        I3)                                                        FA33990
  920 FORMAT (//,' CASE 2B:, GIVEN H1, ANGLE, RANGE',//,10X,             FA34000
     *        'NOTE: H2 IS COMPUTED FROM H1, ANGLE, AND RANGE ',         FA34010
     *        'ASSUMING REFRACTION')                                     FA34020
  925 FORMAT (//,10X,'CALCULATED H2 IS LESS THAN ZERO:',/,10X,           FA34030
     *        'RESET H2 = 0.0 AND RANGE = ',F10.3)                       FA34040
  930 FORMAT (//,' CASE 2C: GIVEN H1, H2, RANGE',//,10X,                 FA34050
     *        'NOTE: ANGLE IS COMPUTED FROM H1, H2, AND RANGE ',         FA34060
     *        'ASSUMING NO REFRACTION')                                  FA34070
  935 FORMAT (///,' SLANT PATH PARAMETERS IN STANDARD FORM',/            FA34080
     *        /,10X,'H1         = ',F12.6,' KM',                                
     *        /,10X,'H2         = ',F12.6,' KM',                                
     *        /,10X,'ANGLE      = ',F12.6,' DEG',                               
     *        /,10X,'sin(ANGLE) = ',F12.6,                                      
     *        /,10X,'PHI        = ',F12.6,' DEG',                               
     *        /,10X,'sin(PHI)   = ',F12.6,                                      
     *        /,10X,'HMIN       = ',F12.6,' KM',                                
     *        /,10X,'LEN        = ',I10)                                        
 937  FORMAT (///,' SLANT PATH PARAMETERS AT SATELLITE',/                       
     *        /,10X,'H_SAT        = ',F12.6,' KM',                              
     *        /,10X,'PHI_SAT      = ',F12.6,' DEG'                              
     *        /,10X,'sin(PHI_SAT) = ',F12.6,                                    
     *        /,10X,'PHI_SAT-PHI  = ',F12.6,' DEG')                             
  940 FORMAT ('0FSCGEO: CASE 3B (H1,HMIN,SPACE): ERROR IN INPUT DATA',   FA34130
     *        //,10X,'H1 = ',F12.6,'    IS LESS THAN HMIN = ',F12.6)     FA34140
  945 FORMAT ('0FSCGEO: ERROR IN INPUT DATA, ITYPE NOT EQUAL TO ',       FA34150
     *        ' 2, OR 3.   ITYPE = ',I10,E23.14)                         FA34160
  950 FORMAT ('0FSCGEO: CASE 2C (H1,H2,RANGE): ERROR IN INPUT DATA',     FA34170
     *        //,10X,'ABS(H1-H2) GT RANGE;  H1 = ',F12.6,'    H2 = ',    FA34180
     *        F12.6,'    RANGE = ',F12.6)                                FA34190
  955 FORMAT ('0FSCGEO: CASE 2A (H1,H2,ANGLE): ERROR IN INPUT DATA',     FA34200
     *        //,10X,'H1 = ',F12.6,'    IS GREATER THAN OR EQUAL TO',    FA34210
     *        ' H2 = ',F12.6,/,10X,'AND ANGLE = ',F12.6,'    IS LESS',   FA34220
     *        ' THAN OR EQUAL TO 90.0')                                  FA34230
  960 FORMAT ('0FSCGEO: ITYPE = 2: SLANT PATH INTERSECTS THE EARTH',     FA34240
     *        ' AND CANNOT REACH H2')                                    FA34250
  965 FORMAT (' FSCGEO:  THE ENTIRE PATH LIES ABOVE THE TOP ZMAX ',      FA34260
     *        'OF THE ATMOSPHERIC PROFILE',//,10X,'ZMAX = ',G12.6,5X,    FA34270
     *        '  H1 = ',G12.6,5X,'  H2 = ',G12.6,'  HMIN = ',G12.6)      FA34280
C                                                                        FA34290
 1000 FORMAT (/3X, 'Ambiguous Inputs:',/3X,'H1 and RANGE are both > 0',         
     *    /3X,'but H2 and ANGLE = 0',//3X,'Path could be 2B or 2C',             
     *    //5X,'will assume 2C',//3X,                                           
     *    'change in FSCGEO if 2B is desired')                                  
      END                                                                FA34300
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE REDUCE (H1,H2,ANGLE,PHI,ITER)                           FA34310
C                                                                        FA34320
C     *****************************************************************  FA34330
C     ZMAX IS THE HIGHEST LEVEL IN THE ATMOSPHERIC PROFILE STORED IN     FA34340
C     COMMON /MDATA/.  IF H1 AND/OR H2 ARE GREATER THAN ZMAX, THIS       FA34350
C     SUBROUTINE REDUCES THEM TO ZMAX AND RESETS ANGLE AND/OR PHI        FA34360
C     AS NECESSARY. THIS REDUCTION IS NECESSARY,FOR EXAMPLE FOR          FA34370
C     SATELLITE ALTITUDES, BECAUSE (1) THE DENSITY PROFILES ARE          FA34380
C     POORLY DEFINED ABOVE ZMAX AND (2) THE CALCULATION TIME FOR         FA34390
C     PATHS ABOVE ZMAX CAN BE EXCESSIVE ( EG. FOR GEOSYNCRONOUS          FA34400
C     ALTITUDES)                                                         FA34410
C     *****************************************************************  FA34420
C                                                                        FA34430
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA34440
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA34450
     *              NLTEFL,LNFIL4,LNGTH4                                 FA34460
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA34480
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA34490
C                                                                        FA34500
      IF (H1.LE.ZMAX.AND.H2.LE.ZMAX) RETURN                              FA34510
      CALL FINDSH (H1,SH,GAMMA)                                          FA34520
      CPATH = ANDEX(H1,SH,GAMMA)*(RE+H1)*SIN(ANGLE/DEG)                  FA34530
      CALL FINDSH (ZMAX,SH,GAMMA)                                        FA34540
      CZMAX = ANDEX(ZMAX,SH,GAMMA)*(RE+ZMAX)                             FA34550
      ANGMAX = 180.0-ASIN(CPATH/CZMAX)*DEG                               FA34560
      IF (H1.LE.ZMAX) GO TO 10                                           FA34570
      H1 = ZMAX                                                          FA34580
      ANGLE = ANGMAX                                                     FA34590
   10 CONTINUE                                                           FA34600
      IF (H2.LE.ZMAX) GO TO 20                                           FA34610
      H2 = ZMAX                                                          FA34620
      PHI = ANGMAX                                                       FA34630
   20 CONTINUE                                                           FA34640
      IF (ITER.EQ.0) WRITE (IPR,900) ZMAX,ANGMAX                         FA34650
C                                                                        FA34660
      RETURN                                                             FA34670
C                                                                        FA34680
  900 FORMAT (///,' FROM SUBROUTINE REDUCE : ',/,10X,'ONE OR BOTH OF',   FA34690
     *        ' H1 AND H2 ARE ABOVE THE TOP OF THE ATMOSPHERIC ',        FA34700
     *        'PROFILE ZMAX = ',F10.3,'  AND HAVE BEEN RESET TO ZMAX.',  FA34710
     *        /,10X,'ANGLE AND/OR PHI HAVE ALSO BEEN RESET TO THE ',     FA34720
     *        'ZENITH ANGLE AT ZMAX = ',F10.3,' DEG')                    FA34730
C                                                                        FA34740
      END                                                                FA34750
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE FDBETA (H1,H2,BETAS,ANGLE,PHI,LEN,HMIN,IERROR)          FA34760
C                                                                        FA34770
C     *****************************************************************  FA34780
C     GIVEN H1,H2,AND BETA (THE EARTH CENTERED ANGLE) THIS SUBROUTINE    FA34790
C     CALCULATES THE INITIAL ZENITH ANGLE AT H1 THROUGH AN ITERATIVE     FA34800
C     PROCEDURE                                                          FA34810
C     *****************************************************************  FA34820
C                                                                        FA34830
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA34840
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA34850
C                                                                        FA34860
      REAL*8           RA,RB,SG,ANGLE1,ANGLE2,BETA,DBETA                !FA34870
C                                                                        FA34880
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA34890
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA34900
     *              NLTEFL,LNFIL4,LNGTH4                                 FA34910
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA34930
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA34940
      COMMON /BNDRY/ ZBND(MXFSC),PBND(MXFSC),TBND(MXFSC),ALORNZ(MXFSC),  FA34950
     *               ADOPP(MXFSC),AVOIGT(MXFSC)                          FA34960
C                                                                        FA34970
      DATA TOLRNC / 5.0E-3 /,ITERMX / 10 /,BETD / 0.04 /                 FA34980
      DATA ZER / 0. /                                                    FA34990
C                                                                        FA35000
      BETA = BETAS                                                       FA35010
      IFLAG = 0                                                          FA35020
      IF (H1.LE.H2) THEN                                                        
          IORDER = 1                                                     FA35040
          HA = H1                                                        FA35050
          HB = H2                                                        FA35060
      ELSE                                                                      
          IORDER = -1                                                    FA35090
          HA = H2                                                        FA35100
          HB = H1                                                        FA35110
      ENDIF                                                                     
C                                                                        FA35130
C     IF AUTOLAYERING SELECTED(IBMAX = 0) THEN SET UP DUMMY              FA35140
C     LBLRTM OUTPUT LAYERS                                               FA35150
C                                                                        FA35160
      IBMSAV = IBMAX                                                     FA35170
      IF (IBMAX.EQ.0) THEN                                                      
          IBMAX = 2                                                      FA35190
          ZBND(1) = ZMIN                                                 FA35200
          ZBND(2) = ZMAX                                                 FA35210
      ENDIF                                                                     
C                                                                        FA35230
C     SET PARAMETER TO SUPRESS CALCULATION OF AMOUNTS                    FA35240
C                                                                        FA35250
      IAMTB = 2                                                          FA35260
C                                                                        FA35270
C     GUESS AT ANGLE, INTEGRATE TO FIND BETA, TEST FOR                   FA35280
C     CONVERGENCE, AND ITERATE                                           FA35290
C     FIRST GUESS AT ANGLE: USE THE GEOMETRIC SOLUTION (NO REFRACTION)   FA35300
C                                                                        FA35310
      WRITE (IPR,900)                                                    FA35320
      ITER = 0                                                           FA35330
      RA = RE+HA                                                         FA35340
      RB = RE+HB                                                         FA35350
      SG = SQRT((HA-HB)**2+4.0*RA*RB*(SIN(BETA/(2.0*DEG)))**2)           FA35360
      ANGLE1 = 180.0-ACOS((HA**2-HB**2+2.0*RE*(HA-HB)+SG**2)                    
     *         /(2.0*RA*SG))*DEG                                         FA35370
      HMIN = HA                                                          FA35380
      IF (ANGLE1.GT.90.0) HMIN = RA*SIN(ANGLE1/DEG)-RE                   FA35390
      HMING = HMIN                                                       FA35400
      ANGLS1 = ANGLE1                                                    FA35410
      CALL FNDHMN (HA,ANGLS1,HB,LEN,ITER,HMIN,PHI,IERROR)                FA35420
      LEN = 0                                                            FA35430
      IF (HMIN.LT.HA) LEN = 1                                            FA35440
      CALL RFPATH (HA,HB,ANGLS1,PHI,LEN,HMIN,IAMTB,RANGE,BETA1,BENDNG)   FA35450
      WRITE (IPR,905) ITER,ANGLS1,BETA,ZER,SG,HMING,ZER,ZER              FA35460
C                                                                        FA35470
C     OBTAIN DERIVATIVE                                                  FA35480
C                                                                        FA35490
      SG = SQRT((HA-HB)**2+4.0*RA*RB*(SIN((BETA+BETD)/(2.0*DEG)))**2)    FA35500
      ANGLEP = 180.0-ACOS((HA**2-HB**2+2.0*RE*(HA-HB)+SG**2)                    
     *         /(2.0*RA*SG))*DEG                                         FA35510
      DANG = ANGLE1-ANGLEP                                               FA35520
      IF (HMIN.LT.0.0) THEN                                              FA35530
          IFLAG = 1                                                      FA35540
          HMIN = 0.0                                                     FA35550
          CALL FNDHMN (HMIN,90.0,HA,LEN,ITER,HMIN,ANGLS1,IERROR)         FA35560
      ENDIF                                                              FA35570
      ITER = 1                                                           FA35580
      LEN = 0                                                            FA35590
      IF (ANGLE1.GT.90.0) LEN = 1                                        FA35600
      CALL FNDHMN (HA,ANGLS1,HB,LEN,ITER,HMIN,PHI,IERROR)                FA35610
      LEN = 0                                                            FA35620
      IF (HMIN.LT.HA) LEN = 1                                            FA35630
      CALL RFPATH (HA,HB,ANGLS1,PHI,LEN,HMIN,IAMTB,RANGE,BETA1,BENDNG)   FA35640
      DBETA = BETA-BETA1                                                 FA35650
      WRITE (IPR,905) ITER,ANGLS1,BETA1,DBETA,RANGE,HMIN,PHI,BENDNG      FA35660
      IF (IFLAG.EQ.1.AND.BETA1.LT.BETA) GO TO 90                         FA35670
   50 CONTINUE                                                           FA35680
      ANGLEP = ANGLE1-DANG                                               FA35690
      LEN = 0                                                            FA35700
      IF (ANGLEP.GT.90.0) LEN = 1                                        FA35710
      CALL FNDHMN (HA,ANGLEP,HB,LEN,ITER,HMIN,PHI,IERROR)                FA35720
      LEN = 0                                                            FA35730
      IF (HMIN.LT.HA) LEN = 1                                            FA35740
      CALL RFPATH (HA,HB,ANGLEP,PHI,LEN,HMIN,IAMTB,RANGE,BETAP,BENDNG)   FA35750
      IF (ABS(BETA1-BETAP).LT.TOLRNC) GO TO 60                           FA35760
      ITER = ITER+1                                                      FA35770
      DC = BETAP-BETA1                                                   FA35780
      DERIV = -DC/BETD                                                   FA35790
      ANGLE2 = ANGLE1+(ANGLE1-ANGLEP)*(BETA-BETA1)/(BETA1-BETAP)         FA35800
      ANGLS2 = ANGLE2                                                    FA35810
      LEN = 0                                                            FA35820
      IF (ANGLE2.GT.90.0) LEN = 1                                        FA35830
      CALL FNDHMN (HA,ANGLS2,HB,LEN,ITER,HMIN,PHI,IERROR)                FA35840
      LEN = 0                                                            FA35850
      IF (HMIN.LT.HA) LEN = 1                                            FA35860
      CALL RFPATH (HA,HB,ANGLS2,PHI,LEN,HMIN,IAMTB,RANGE,BETA2,BENDNG)   FA35870
      DBETA = BETA-BETA2                                                 FA35880
      WRITE (IPR,905) ITER,ANGLS2,BETA2,DBETA,RANGE,HMIN,PHI,BENDNG      FA35890
      IF (BETA2.LT.BETA.AND.HMIN.LT.0.0) GO TO 90                        FA35900
      ANGLE1 = ANGLE2                                                    FA35910
      ANGLS1 = ANGLE1                                                    FA35920
      BETA1 = BETA2                                                      FA35930
      IF (ABS(BETA-BETA2).LT.TOLRNC) GO TO 70                            FA35940
      IF (ITER.GT.ITERMX) GO TO 100                                      FA35950
      GO TO 50                                                           FA35960
   60 ANGLE2 = ANGLEP                                                    FA35970
      ANGLS2 = ANGLE2                                                    FA35980
      BETA = BETAP                                                       FA35990
   70 CONTINUE                                                           FA36000
      IF (HMIN.LT.0.0) GO TO 90                                          FA36010
C                                                                        FA36020
C     CONVERGED TO A SOLUTION                                            FA36030
C                                                                        FA36040
      ANGLE = ANGLE2                                                     FA36050
      BETA = BETA2                                                       FA36060
C                                                                        FA36070
C     ASSIGN ANGLE AND PHI TO PROPER H1 AND H2                           FA36080
C                                                                        FA36090
      IF (IORDER.NE.1) THEN                                                     
          TEMP = PHI                                                     FA36110
          PHI = ANGLE                                                    FA36120
          ANGLE = TEMP                                                   FA36130
      ENDIF                                                                     
      IBMAX = IBMSAV                                                     FA36150
      BETAS = BETA                                                       FA36160
C                                                                        FA36170
      RETURN                                                             FA36180
C                                                                        FA36190
C     ERROR MESSAGES                                                     FA36200
C                                                                        FA36210
   90 CONTINUE                                                           FA36220
      WRITE (IPR,910)                                                    FA36230
      GO TO 110                                                          FA36240
  100 CONTINUE                                                           FA36250
      WRITE (IPR,915) H1,H2,BETA,ITER,ANGLE1,BETA1,ANGLE2,BETA2          FA36260
C                                                                        FA36270
  110 IERROR = 1                                                         FA36280
C                                                                        FA36290
      RETURN                                                             FA36300
C                                                                        FA36310
  900 FORMAT (///,' CASE 2D: GIVEN H1, H2,  BETA:',//,                   FA36320
     *        ' ITERATE AROUND ANGLE UNTIL BETA CONVERGES',//,           FA36330
     *        ' ITER    ANGLE',T21,'BETA',T30,'DBETA',T40,'RANGE',       FA36340
     *        T51,'HMIN',T61,'PHI',T70,'BENDING',/,T10,'(DEG)',T21,      FA36350
     *        '(DEG)',T30,'(DEG)',T41,'(KM)',T51,'(KM)',T60,'(DEG)',     FA36360
     *        T71,'(DEG)',/)                                             FA36370
  905 FORMAT (I5,3F10.4,2F10.3,2F10.4)                                   FA36380
  910 FORMAT ('0FDBETA, CASE 2D(H1,H2,BETA): REFRACTED TANGENT ',        FA36390
     *        'HEIGHT IS LESS THAN ZERO-PATH INTERSECTS THE EARTH',      FA36400
     *        //,10X,'BETA IS TOO LARGE FOR THIS H1 AND H2')             FA36410
  915 FORMAT ('0FDBETA, CASE 2D (H1,H2,BETA): SOLUTION DID NOT ',        FA36420
     *        ' CONVERGE',//,10X,'H1 = ',F12.6,'    H2 = ',F12.6,        FA36430
     *        '    BETA = ',F12.6,'    ITERATIONS = ',I4,//,10X,         FA36440
     *        'LAST THREE ITERATIONS ',//,(10X,'ANGLE = ',F15.9,         FA36450
     *        '    BETA = ',F15.9))                                      FA36460
C                                                                        FA36470
      END                                                                FA36480
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE FNDHMN (H1,ANGLE,H2,LEN,ITER,HMIN,PHI,IERROR)           FA36490
C                                                                        FA36500
C     *****************************************************************  FA36510
C     THIS SUBROUTINE CALCULATES THE MINIMUM ALTITUDE HMIN ALONG         FA36520
C     THE REFRACTED PATH AND THE FINAL ZENITH ANGLE PHI.                 FA36530
C     THE PARAMETER LEN INDICATES WHETHER THE PATH GOES THROUGH          FA36540
C     A TANGENT HEIGHT (LEN=1) OR NOT (LEN=0).  IF ANGLE > 90 AND        FA36550
C     H1 > H2, THEN LEN CAN EITHER BE 1 OR 0, AND THE CHOICE IS          FA36560
C     LEFT TO THE USER.                                                  FA36570
C     THE (INDEX OF REFRACTION - 1.0) IS MODELED AS AN EXPONENTIAL       FA36580
C     BETWEEN THE LAYER BOUNDARIES, WITH A SCALE HEIGHT SH AND AN        FA36590
C     AMOUNT AT THE GROUND GAMMA.                                        FA36600
C     CPATH IS THE REFRACTIVE CONSTANT FOR THIS PATH AND                 FA36610
C     EQUALS  INDEX(H1)*(RE+H1)*SIN(ANGLE).                              FA36620
C     *****************************************************************  FA36630
C                                                                        FA36640
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA36650
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA36660
     *              NLTEFL,LNFIL4,LNGTH4                                 FA36670
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA36690
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA36700
C                                                                        FA36710
      REAL*8           CPATH,CRFRCT,ANDEXD,SH,GAMMA,CT1,CTP,                    
     *                 CH2,CMIN                                                 
C                                                                               
      DATA DH / 0.2 /,ETA / 5.0E-7 /                                    >FA36720
C                                                                        FA36730
C>    ETA MAY BE TOO SMALL FOR SOME COMPUTERS. TRY 1.0E-7 FOR 32 BIT    >FA36740
C>    WORD MACHINES                                                     >FA36750
C                                                                        FA36760
      CRFRCT(H) = (RE+H)*ANDEXD(H,SH,GAMMA)                              FA36770
      N = 0                                                              FA36780
      CALL FNDSHD (H1,SH,GAMMA)                                          FA36790
      CPATH = CRFRCT(H1)*SIN(ANGLE/DEG)                                  FA36800
      CALL FNDSHD (H2,SH,GAMMA)                                          FA36810
      CH2 = CRFRCT(H2)                                                   FA36820
      IF (ABS(CPATH/CH2).GT.1.0) GO TO 70                                FA36830
      IF (ANGLE.LE.90.0) THEN                                            FA36840
          LEN = 0                                                        FA36850
          HMIN = H1                                                      FA36860
          GO TO 60                                                       FA36870
      ENDIF                                                              FA36880
      IF (H1.LE.H2) LEN = 1                                              FA36890
      IF (LEN.NE.1) THEN                                                 FA36900
          LEN = 0                                                        FA36910
          HMIN = H2                                                      FA36920
          GO TO 60                                                       FA36930
      ENDIF                                                              FA36940
C                                                                        FA36950
C     LONG PATH THROUGH A TANGENT HEIGHT.                                FA36960
C     SOLVE ITERATIVELY FOR THE TANGENT HEIGHT HT.                       FA36970
C     HT IS THE HEIGHT FOR WHICH  INDEX(HT)*(RE+HT) = CPATH.             FA36980
C                                                                        FA36990
      CALL FNDSHD (0.0,SH,GAMMA)                                         FA37000
      CMIN = CRFRCT(0.0)                                                 FA37010
C                                                                        FA37020
C     FOR BETA CASES (ITER>0), ALLOW FOR HT < 0.0                        FA37030
C                                                                        FA37040
      IF (ITER.EQ.0.AND.CPATH.LT.CMIN) GO TO 50                          FA37050
      HT1 = H1*SIN(ANGLE/DEG)+(SIN(ANGLE/DEG)-1.0)*RE                    FA37060
C                                                                        FA37070
C     ITERATE TO FIND HT                                                 FA37080
C                                                                        FA37090
   30 CONTINUE                                                           FA37100
      N = N+1                                                            FA37110
      CALL FNDSHD (HT1,SH,GAMMA)                                         FA37120
      CT1 = CRFRCT(HT1)                                                  FA37130
      IF (ABS((CPATH-CT1)/CPATH).LT.ETA) GO TO 40                        FA37140
      IF (N.GT.15) GO TO 80                                              FA37150
      HTP = HT1-DH                                                       FA37160
      CALL FNDSHD (HTP,SH,GAMMA)                                         FA37170
      CTP = CRFRCT(HTP)                                                  FA37180
      DERIV=(CT1-CTP)/DH                                                        
      HT1=HT1+(CPATH-CT1)/DERIV                                                 
      GO TO 30                                                           FA37230
   40 CONTINUE                                                           FA37240
      HMIN=HT1                                                                  
      GO TO 60                                                           FA37270
   50 CONTINUE                                                           FA37280
C                                                                        FA37290
C     TANGENT PATH INTERSECTS EARTH                                      FA37300
C                                                                        FA37310
      H2 = 0.0                                                           FA37320
      HMIN = 0.0                                                         FA37330
      LEN = 0                                                            FA37340
      CH2 = CMIN                                                         FA37350
      WRITE (IPR,900) H1,ANGLE                                           FA37360
   60 CONTINUE                                                           FA37370
C                                                                        FA37380
C     CALCULATE THE ZENITH ANGLE PHI AT H2                               FA37390
C                                                                        FA37400
      PHI = ASIN(CPATH/CH2)*DEG                                          FA37410
      IF (ANGLE.LE.90.0.OR.LEN.EQ.1) PHI = 180.0-PHI                     FA37420
C                                                                        FA37430
      RETURN                                                             FA37440
C                                                                        FA37450
C     H2 LT TANGENT HEIGHT FOR THIS H1 AND ANGLE                         FA37460
C                                                                        FA37470
   70 CONTINUE                                                           FA37480
      WRITE (IPR,905)                                                    FA37490
      IERROR = 2                                                         FA37500
C                                                                        FA37510
      RETURN                                                             FA37520
C                                                                        FA37530
   80 CONTINUE                                                           FA37540
      DC = CPATH-CT1                                                     FA37550
      WRITE (IPR,910) N,CPATH,CT1,DC,HT1                                 FA37560
C                                                                        FA37570
      STOP ' FNDHMN '                                                    FA37580
C                                                                        FA37590
  900 FORMAT (///,' TANGENT PATH WITH H1 = ',F10.3,' AND ANGLE = ',      FA37600
     *        F10.3,' INTERSECTS THE EARTH',//,10X,                      FA37610
     *        'H2 HAS BEEN RESET TO 0.0 AND LEN TO 0')                   FA37620
  905 FORMAT ('0H2 IS LESS THAN THE TANGENT HEIGHT FOR THIS PATH ',      FA37630
     *        'AND CANNOT BE REACHED')                                   FA37640
  910 FORMAT (///,'0FROM SUBROUTINE FNDHMN :',//,10X,                    FA37650
     *        'THE PROCEEDURE TO FIND THE TANGENT HEIGHT DID NOT ',      FA37660
     *        'CONVERG AFTER ',I3,'  ITERATIONS',//,10X,'CPATH   = ',    FA37670
     *        F12.5,' KM',//,10X,'CT1     = ',F12.5,' KM',//,10X,        FA37680
     *        'DC      = ',E12.3,' KM',//,10X,'HT1     = ',F12.5,' KM')  FA37690
C                                                                        FA37700
      END                                                                FA37710
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE FINDSH (H,SH,GAMMA)                                     FA37720
C                                                                        FA37730
C     *****************************************************************  FA37740
C     GIVEN AN ALTITUDE H, THIS SUBROUTINE FINDS THE LAYER BOUNDARIES    FA37750
C     Z(I1) AND Z(I2) WHICH CONTAIN H,  THEN CALCULATES THE SCALE        FA37760
C     HEIGHT (SH) AND THE VALUE AT THE GROUND (GAMMA+1) FOR THE          FA37770
C     REFRACTIVITY (INDEX OF REFRACTION -1)                              FA37780
C     *****************************************************************  FA37790
C                                                                        FA37800
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA37810
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA37820
C                                                                        FA37830
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA37840
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA37850
     *              NLTEFL,LNFIL4,LNGTH4                                 FA37860
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA37880
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA37890
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA37900
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA37910
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA37920
C                                                                        FA37930
      CHARACTER*8      HMOD                                             &FA37940
C                                                                        FA37950
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),   FA37970
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD) ,       FA37980
     *       AMTP(MXMOL,MXPDIM)                                          FA37990
C                                                                        FA38000
      DO 10 IM = 2, IMMAX                                                FA38010
         I2 = IM                                                         FA38020
         IF (ZMDL(IM).GE.H) GO TO 20                                     FA38030
   10 CONTINUE                                                           FA38040
      I2 = IMMAX                                                         FA38050
   20 CONTINUE                                                           FA38060
      I1 = I2-1                                                          FA38070
      CALL SCALHT (ZMDL(I1),ZMDL(I2),RFNDXM(I1),RFNDXM(I2),SH,GAMMA)     FA38080
C                                                                        FA38090
      RETURN                                                             FA38100
C                                                                        FA38110
      END                                                                FA38120
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE SCALHT (Z1,Z2,RFNDX1,RFNDX2,SH,GAMMA)                   FA38130
C                                                                        FA38140
C     *****************************************************************  FA38150
C     THIS SUBROUTINE CALCULATES THE SCALE HEIGHT SH OF THE (INDEX OF    FA38160
C     REFRACTION-1.0) FROM THE VALUES OF THE INDEX AT THE ALTITUDES Z1   FA38170
C     AND Z2 ( Z1 < Z2). IT ALSO CALCULATES THE EXTRAPOLATED VALUE       FA38180
C     GAMMA OF THE (INDEX-1.0) AT Z = 0.0                                FA38190
C     *****************************************************************  FA38200
C                                                                        FA38210
      RF1 = RFNDX1+1.0E-20                                               FA38220
      RF2 = RFNDX2+1.0E-20                                               FA38230
      RATIO = RF1/RF2                                                    FA38240
      IF (ABS(RATIO-1.0).LT.1.0E-05) GO TO 10                            FA38250
      SH = (Z2-Z1)/ LOG(RATIO)                                           FA38260
      GAMMA = RF1*(RF2/RF1)**(-Z1/(Z2-Z1))                               FA38270
      GO TO 20                                                           FA38280
   10 CONTINUE                                                           FA38290
C                                                                        FA38300
C     THE VARIATION IN THE INDEX OF REFRACTION WITH HEIGHT IS            FA38310
C     INSIGNIFICANT OR ZERO                                              FA38320
C                                                                        FA38330
      SH = 0.0                                                           FA38340
      GAMMA = RFNDX1                                                     FA38350
   20 CONTINUE                                                           FA38360
C                                                                        FA38370
      RETURN                                                             FA38380
C                                                                        FA38390
      END                                                                FA38400
      FUNCTION ANDEX (H,SH,GAMMA)                                        FA38410
C                                                                        FA38420
C     *****************************************************************  FA38430
C     COMPUTES THE INDEX OF REFRACTION AT HEIGHT H, SH IS THE            FA38440
C     SCALE HEIGHT, GAMMA IS THE VALUE AT H=0 OF THE REFRACTIVITY =      FA38450
C     INDEX-1                                                            FA38460
C     *****************************************************************  FA38470
C                                                                        FA38480
      IF (SH.EQ.0.0) THEN                                                FA38490
         ANDEX = 1.0+GAMMA                                               FA38500
      ELSE                                                               FA38510
         ANDEX = 1.0+GAMMA*EXP(-H/SH)                                    FA38520
      ENDIF                                                              FA38530
C                                                                        FA38540
      RETURN                                                             FA38550
C                                                                        FA38560
      END                                                                FA38570
      FUNCTION RADREF (H,SH,GAMMA)                                       FA38580
C                                                                        FA38590
C     *****************************************************************  FA38600
C     COMPUTES THE RADIUS OF CURVATURE OF THE REFRACTED RAY FOR          FA38610
C     A HORIZONTAL PATH.  RADREF = ANDEX/ D(ANDEX)/D(RADIUS)             FA38620
C     *****************************************************************  FA38630
C                                                                        FA38640
      DATA BIGNUM / 1.0E36 /                                             FA38650
C                                                                        FA38660
      IF (SH.EQ.0.0) GO TO 10                                            FA38670
      RADREF = SH*(1.0+EXP(H/SH)/GAMMA)                                  FA38680
C                                                                        FA38690
      RETURN                                                             FA38700
C                                                                        FA38710
   10 RADREF = BIGNUM                                                    FA38720
C                                                                        FA38730
      RETURN                                                             FA38740
C                                                                        FA38750
      END                                                                FA38760
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE RFPATH (H1,H2,ANGLE,PHI,LEN,HMIN,IAMT,RANGE,BETA,       FA38770
     *                   BENDNG)                                         FA38780
C                                                                               
C     -------------------------------------------------------------             
C     This routine was modified for LBLRTM to reflect changes                   
C     implemented in MODTRAN to solve problems with inconsistent                
C     path parameters.                                                          
C     It was also modified to eliminate GOTO statements in order to             
C     make the program easier to understand.                                    
C     These changes were obtained from H. Snell (March, 1996).                  
C     -------------------------------------------------------------             
C                                                                        FA38790
C     *****************************************************************  FA38800
C     THIS SUBROUTINE TRACES THE REFRACTED RAY FROM H1 WITH AN           FA38810
C     INITIAL ZENITH ANGLE ANGLE TO H2 WHERE THE ZENITH ANGLE IS PHI,    FA38820
C     AND CALCULATES THE ABSORBER AMOUNTS (IF IAMT.EQ.1) ALONG           FA38830
C     THE PATH.  IT STARTS FROM THE LOWEST POINT ALONG THE PATH          FA38840
C     (THE TANGENT HEIGHT HMIN IF LEN = 1 OR HA = MIN(H1,H2) IF LEN = 0  FA38850
C     AND PROCEEDS TO THE HIGHEST POINT.  BETA AND RANGE ARE THE         FA38860
C     EARTH CENTERED ANGLE AND THE TOTAL DISTANCE RESPECTIVELY           FA38870
C     FOR THE REFRACTED PATH FROM H1 TO H2, AND BENDNG IS THE TOTAL      FA38880
C     BENDING ALONG THE PATH                                             FA38890
C     *****************************************************************  FA38900
C                                                                        FA38910
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA38920
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA38930
C                                                                        FA38940
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA38950
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA38960
     *              NLTEFL,LNFIL4,LNGTH4                                 FA38970
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA38990
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA39000
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA39010
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA39020
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA39030
C                                                                        FA39040
      CHARACTER*8      HMOD                                             &FA39050
      REAL*8           DS,DBEND,S,SINAI,COSAI,CPATH,ANDEXD,SH,GAMMA             
C                                                                        FA39060
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                        FA39110
      CHARACTER*2 HLOW(2)                                                FA39120
      DATA HLOW / 'H1','H2'/                                             FA39130
      DATA I_2/2/                                                               
C                                                                        FA39140
C     REORDER H1 AND H2 TO HA AND HB (HA .LE. HB)                        FA39150
C                                                                        FA39160
      IF (H1.LE.H2) THEN                                                        
          IORDER = 1                                                     FA39180
          HA = H1                                                        FA39190
          HB = H2                                                        FA39200
          ANGLEA = ANGLE                                                 FA39210
      ELSE                                                                      
          IORDER = -1                                                    FA39240
          HA = H2                                                        FA39250
          HB = H1                                                        FA39260
          ANGLEA = PHI                                                   FA39270
      ENDIF                                                                     
C                                                                        FA39290
C     MERGE THE ATMOSPHERIC PROFILE STORED IN ZMDL WITH H1,H2,(HMIN) AN  FA39300
C     THE BOUNDARIES ZBND                                                FA39310
C                                                                        FA39320
      CALL AMERGE (H1,H2,HMIN,LEN)                                       FA39330
      IF (IAMT.EQ.1.AND.NOPRNT.ge.0) WRITE (IPR,900)                     FA39340
C                                                                        FA39350
C     CALCULATE CPATH SEPERATELY FOR LEN = 0,1                           FA39360
C                                                                        FA39370
      IF (LEN.EQ.0) THEN                                                 FA39380
          CALL FNDSHD (HA,SH,GAMMA)                                      FA39390
          CPATH = (RE+HA)*ANDEXD(HA,SH,GAMMA)*SIN(ANGLEA/DEG)            FA39400
      ELSE                                                               FA39410
          CALL FNDSHD (HMIN,SH,GAMMA)                                    FA39420
          CPATH = (RE+HMIN)*ANDEXD(HMIN,SH,GAMMA)                        FA39430
      ENDIF                                                              FA39440
C                                                                        FA39450
      BETA = 0.0                                                         FA39460
      S = 0.0                                                            FA39470
      BENDNG = 0.0                                                       FA39480
      IF (LEN.EQ.1) THEN                                                 FA39490
C                                                                        FA39500
C     TANGENT PATH                                                       FA39510
C                                                                        FA39520
          IF (IORDER.EQ.-1) THEN                                                
              IHLOW = 2                                                         
          ELSE                                                                  
              IHLOW = 1                                                  FA39530
          ENDIF                                                          FA39540
          IF (IAMT.EQ.1.AND.NOPRNT.ge.0) WRITE (IPR,905) HLOW(IHLOW)     FA39550
          SINAI = 1.0                                                    FA39560
          COSAI = 0.0                                                    FA39570
          THETA = 90.0                                                   FA39580
      ELSE                                                               FA39590
C                                                                        FA39600
C     SHORT PATH                                                         FA39610
C                                                                        FA39620
C     ANGLEA IS THE ZENITH ANGLE AT HA IN DEG                            FA39650
C     SINAI IS SIN OF THE INCIDENCE ANGLE                                FA39660
C     COSAI IS CARRIED SEPERATELY TO AVOID A PRECISION PROBLEM           FA39670
C     WHEN SINAI IS CLOSE TO 1.0                                         FA39680
C                                                                        FA39690
          THETA = ANGLEA                                                 FA39700
          IF (ANGLEA.LE.45.0) THEN                                       FA39710
              SINAI = SIN(ANGLEA/DEG)                                    FA39720
              COSAI = -COS(ANGLEA/DEG)                                   FA39730
          ELSE                                                           FA39740
              SINAI = COS((90.0-ANGLEA)/DEG)                             FA39760
              COSAI = -SIN((90.0-ANGLEA)/DEG)                            FA39770
          ENDIF                                                                 
          IF (IORDER.EQ.-1) THEN                                         FA39780
              IHLOW = 2                                                         
          ELSE                                                                  
              IHLOW = 1                                                  FA39790
          ENDIF                                                                 
          IHIGH = MOD(IHLOW,I_2)+1                                         FA398
          IF (IAMT.EQ.1.AND.NOPRNT.ge.0)                                 FA39820
     *        WRITE (IPR,910) HLOW(IHLOW),HLOW(IHIGH)                    FA39830
      ENDIF                                                              FA39840
C                                                                        FA39850
C     LOOP OVER THE LAYERS                                               FA39860
C                                                                        FA39870
      J2 = IPMAX-1                                                       FA39880
      DO 100 J = 1, J2                                                   FA39890
         CALL SCLHTD (ZPTH(J),ZPTH(J+1),RFNDXP(J),RFNDXP(J+1),SH,GAMMA)  FA39900
         CALL ALAYER (J,SINAI,COSAI,CPATH,SH,GAMMA,IAMT,DS,DBEND)        FA39910
         DBEND = DBEND*DEG                                               FA39920
         PHI = ASIN(SINAI)*DEG                                           FA39930
         DBETA = THETA-PHI+DBEND                                         FA39940
         PHI = 180.0-PHI                                                 FA39950
         S = S+DS                                                        FA39960
         BENDNG = BENDNG+DBEND                                           FA39970
         BETA = BETA+DBETA                                               FA39980
         IF (IAMT.EQ.1) THEN                                                    
             PBAR = PPSUM(J)/RHOPSM(J)                                   FA40000
             TBAR = TPSUM(J)/RHOPSM(J)                                   FA40010
             RHOBAR = RHOPSM(J)/DS                                       FA40020
             IF (NOPRNT.ge.0) WRITE (IPR,915) J,ZPTH(J),ZPTH(J+1),       FA40030
     *            THETA,DS,S,DBETA,BETA,PHI,DBEND,BENDNG,PBAR,           FA40040
     *            TBAR,RHOBAR                                            FA40050
         ENDIF                                                           FA40060
         THETA = 180.0-PHI                                               FA40070
C                                                                               
         IF (LEN.EQ.1) THEN                                              FA40080
C                                                                        FA40090
C            For tangent paths, double the quantities BENDNG,BETA,       FA40100
C            and S for the symmetric part of the path                    FA40110
C                                                                        FA40120
             IF ((J+1).EQ.IPHMID) THEN                                   FA40130
                 BENDNG = 2.0*BENDNG                                     FA40140
                 BETA = 2.0*BETA                                         FA40150
                 S = 2.0*S                                               FA40160
                 IF (IAMT.EQ.1.AND.NOPRNT.ge.0)                                 
     *                         WRITE (IPR,920) S,BETA,BENDNG             FA40170
                 IF (IPHMID.NE.IPMAX) THEN                               FA40180
                     IF (IORDER.EQ.-1) THEN                              FA40190
                         IHLOW = 2                                       FA40200
                     ELSE                                                       
                         IHLOW = 1                                              
                     ENDIF                                                      
                     IHIGH = MOD(IHLOW,I_2)+1                              FA402
                     IF (IAMT.EQ.1.AND.NOPRNT.ge.0)                      FA40220
     *                   WRITE (IPR,910) HLOW(IHLOW),HLOW(IHIGH)         FA40230
                 ENDIF                                                          
             ENDIF                                                              
         ENDIF                                                                  
  100 CONTINUE                                                           FA40240
      IF (IORDER.EQ.-1) PHI = ANGLEA                                     FA40250
      RANGE = S                                                          FA40260
C                                                                        FA40270
      RETURN                                                             FA40280
C                                                                        FA40290
  900 FORMAT ('1CALCULATION OF THE REFRACTED PATH THROUGH THE ',         FA40300
     *        'ATMOSPHERE',///,T5,'I',T14,'ALTITUDE',T30,'THETA',T38,    FA40310
     *        'DRANGE',T47,'RANGE',T57,'DBETA',T65,'BETA',T76,'PHI',     FA40320
     *        T84,'DBEND',T91,'BENDING',T102,'PBAR',T111,'TBAR',T119,    FA40330
     *        'RHOBAR',/,T11,'FROM',T22,'TO',/,T11,'(KM)',T21,'(KM)',    FA40340
     *        T30,'(DEG)',T39,'(KM)',T48,'(KM)',T57,'(DEG)',T65,         FA40350
     *        '(DEG)',T75,'(DEG)',T84,'(DEG)',T92,'(DEG)',T102,'(MB)',   FA40360
     *        T112,'(K)',T117,'(MOL CM-3)',/)                            FA40370
  905 FORMAT (' ',T10,'TANGENT',T20,A2,/,T10,'HEIGHT',/)                 FA40380
  910 FORMAT (' ',T14,A2,' TO ',A2,/)                                    FA40390
  915 FORMAT (' ',I4,2F10.3,10F9.3,1PE9.2)                               FA40400
  920 FORMAT ('0',T10,'DOUBLE RANGE, BETA, BENDING',/,T10,               FA40410
     *        'FOR SYMMETRIC PART OF PATH',T44,F9.3,T62,F9.3,T89,        FA40420
     *        F9.3,/)                                                    FA40430
C                                                                        FA40440
      END                                                                FA40450
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE AMERGE (H1,H2,HMIN,LEN)                                 FA40460
C                                                                        FA40470
C     *****************************************************************  FA40480
C     AMERGE CREATES A SET OF LAYER BOUNDARIES ZOUT WHICH INCLUDES       FA40490
C     HMIN, (HMID), HMAX AND ALL OF ZBND BETWEEN HMIN AND HAMX.          FA40500
C     ZOUT DEFINES THE LAYERS FOR THE LBLRTM CALCULATION.                FA40510
C     ZOUT IS THEN MERGED WITH THE ATMOSPHERIC PROFILE IN ZMDL INTO ZPT  FA40520
C     INTERPOLATING TO THE LEVELS ZOUT WHEN NECESSARY.  THE RAY          FA40530
C     TRACE IS CALCULATED USING THE PROFILE IN ZPTH.                     FA40540
C     *****************************************************************  FA40550
C                                                                        FA40560
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA40570
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA40580
C                                                                        FA40590
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA40600
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA40610
     *              NLTEFL,LNFIL4,LNGTH4                                 FA40620
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA40640
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA40650
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA40680
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA40690
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA40700
C                                                                        FA40710
      CHARACTER*8      HMOD                                             &FA40720
C                                                                        FA40730
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                        FA40780
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FA40790
      COMMON /BNDRY/ ZBND(MXFSC),PBND(MXFSC),TBND(MXFSC),ALORNZ(MXFSC),  FA40800
     *               ADOPP(MXFSC),AVOIGT(MXFSC)                          FA40810
      COMMON /ZOUTP/ ZOUT(MXLAY),SOUT(MXLAY),RHOSUM(MXLAY),              FA40820
     *               AMTTOT(MXMOL),AMTCUM(MXMOL),ISKIP(MXMOL)            FA40830
      DIMENSION ZH(3)                                                    FA40840
C                                                                        FA40850
      DATA TOL / 5.E-4 /                                                 FA40860
C                                                                        FA40870
      DATA I_2/2/                                                               
C                                                                               
C     HMID .EQ. MINIMUM OF H1, H2                                        FA40880
C                                                                        FA40890
      HMID =   MIN(H1,H2)                                                FA40900
      HMAX =   MAX(H1,H2)                                                FA40910
      IHMAX = 2                                                          FA40920
      ZH(1) = HMIN                                                       FA40930
      IF (LEN.EQ.0) THEN                                                 FA40940
          ZH(2) = HMAX                                                   FA40950
      ELSE                                                               FA49060
          ZH(2) = HMID                                                   FA40980
          IF (ABS(H1-H2).LT.TOL) H1 = H2                                 FA40990
          IF (H1.NE.H2) THEN                                             FA41000
              IHMAX = 3                                                  FA41010
              ZH(3) = HMAX                                               FA41020
          ENDIF                                                          FA41030
      ENDIF                                                                     
C                                                                        FA41040
C     MERGE ZH AND ZBND BETWEEN ZH(1) AND ZH(IHMAX) TO CREAT ZOUT        FA41050
C                                                                        FA41060
      ZOUT(1) = ZH(1)                                                    FA41070
      DO 30 I1 = 1, IBMAX                                                FA41080
         IF (ABS(ZBND(I1)-ZH(1)).LT.TOL) ZH(1) = ZBND(I1)                FA41090
         IF (ZBND(I1).GT.ZH(1)) GO TO 40                                 FA41100
   30 CONTINUE                                                           FA41110
      I1 = IBMAX                                                         FA41120
   40 CONTINUE                                                           FA41130
C                                                                        FA41140
C     ZBND(I1) IS SMALLEST ZBND .GT. ZH(1)                               FA41150
C                                                                        FA41160
      IOUT = 1                                                           FA41170
      IB = I1                                                            FA41180
      IH = 2                                                             FA41190
   50 CONTINUE                                                           FA41200
      IOUT = IOUT+1                                                      FA41210
      IF (IB.GT.IBMAX) GO TO 60                                          FA41220
      IF (ABS(ZBND(IB)-ZH(IH)).LT.TOL) ZH(IH) = ZBND(IB)                 FA41230
      IF (ZBND(IB).LT.ZH(IH)) GO TO 70                                   FA41240
      IF (ZBND(IB).EQ.ZH(IH)) IB = IB+1                                  FA41250
C                                                                        FA41260
C     INSERT ZH(IH)                                                      FA41270
C                                                                        FA41280
   60 CONTINUE                                                           FA41290
      ZOUT(IOUT) = ZH(IH)                                                FA41300
      IH = IH+1                                                          FA41310
      IF (IH.GT.IHMAX) GO TO 80                                          FA41320
      GO TO 50                                                           FA41330
C                                                                        FA41340
C     INSERT ZBND(IB)                                                    FA41350
C                                                                        FA41360
   70 CONTINUE                                                           FA41370
      ZOUT(IOUT) = ZBND(IB)                                              FA41380
      IB = IB+1                                                          FA41390
      GO TO 50                                                           FA41400
C                                                                        FA41410
   80 CONTINUE                                                           FA41420
      IOUTMX = IOUT                                                      FA41430
C                                                                        FA41440
C     NOW MERGE ZOUT AND ZMDL INTO ZPTH (FROM ZOUT(1) TO ZOUT(IOUTMX))   FA41450
C     AND INTERPOLATE PRESSURE, TEMPERATURE, AND DENSITY WHEN            FA41460
C     NECESSARY                                                          FA41470
C                                                                        FA41480
C     FIND SMALLEST ZMDL .GT. HMIN                                       FA41490
C                                                                        FA41500
      DO 90 IM = 1, IMMAX                                                FA41510
         IF (ZMDL(IM).GE.HMIN) GO TO 100                                 FA41520
   90 CONTINUE                                                           FA41530
      WRITE (IPR,900) HMIN                                               FA41540
      STOP ' AMERGE - HMIN '                                                    
  100 CONTINUE                                                           FA41550
      IPHMID = 0                                                         FA41560
      IP = 0                                                             FA41570
      IOUT = 1                                                           FA41580
  110 CONTINUE                                                           FA41590
      IP = IP+1                                                          FA41600
      IF (IP.GT.IPDIM) THEN                                              FA41610
          WRITE (IPR,905) IPDIM                                                 
          STOP ' AMERGE - IPDIM '                                               
      ENDIF                                                                     
      IF (IM.GT.IMMAX) GO TO 130                                         FA41620
      IF (ABS(ZOUT(IOUT)-ZMDL(IM)).LT.TOL) ZMDL(IM) = ZOUT(IOUT)         FA41630
      IF (ZOUT(IOUT).LT.ZMDL(IM)) GO TO 130                              FA41640
      IF (ZOUT(IOUT).EQ.ZMDL(IM)) IOUT = IOUT+1                          FA41650
C                                                                        FA41660
C     INSERT ZMDL(IM)                                                    FA41670
C                                                                        FA41680
      ZPTH(IP) = ZMDL(IM)                                                FA41690
      PP(IP) = PM(IM)                                                    FA41700
      TP(IP) = TM(IM)                                                    FA41710
      RFNDXP(IP) = RFNDXM(IM)                                            FA41720
      DO 120 K = 1, NMOL                                                 FA41730
         DENP(K,IP) = DENM(K,IM)                                         FA41740
  120 CONTINUE                                                           FA41750
      IM = IM+1                                                          FA41760
      IF (ABS(ZPTH(IP)-HMID).LT.TOL) HMID = ZPTH(IP)                     FA41770
      IF (ZPTH(IP).EQ.HMID) IPHMID = IP                                  FA41780
      IF (ABS(ZPTH(IP)-ZOUT(IOUTMX)).LT.TOL) ZOUT(IOUTMX) = ZPTH(IP)     FA41790
      IF (ZPTH(IP).EQ.ZOUT(IOUTMX)) GO TO 150                            FA41800
      GO TO 110                                                          FA41810
C                                                                        FA41820
C     INSERT LEVEL FROM ZOUT(IOUT) AND INTERPOLATE                       FA41830
C                                                                        FA41840
  130 CONTINUE                                                           FA41850
      ZPTH(IP) = ZOUT(IOUT)                                              FA41860
      JM = IM                                                            FA41870
      JM = MAX(JM,I_2)                                                     FA418
      A = (ZOUT(IOUT)-ZMDL(JM-1))/(ZMDL(JM)-ZMDL(JM-1))                  FA41890
      CALL EXPINT (PP(IP),PM(JM-1),PM(JM),A)                             FA41900
      TP(IP) = TM(JM-1)+(TM(JM)-TM(JM-1))*A                              FA41910
      CALL EXPINT (RFNDXP(IP),RFNDXM(JM-1),RFNDXM(JM),A)                 FA41920
      DO 140 K = 1, NMOL                                                 FA41930
         CALL EXPINT (DENP(K,IP),DENM(K,JM-1),DENM(K,JM),A)              FA41940
  140 CONTINUE                                                           FA41950
      IF (ABS(ZPTH(IP)-HMID).LT.TOL) ZPTH(IP) = HMID                     FA41960
      IF (ZPTH(IP).EQ.HMID) IPHMID = IP                                  FA41970
      IOUT = IOUT+1                                                      FA41980
      IF (ABS(ZPTH(IP)-ZOUT(IOUTMX)).LT.TOL) ZPTH(IP) = ZOUT(IOUTMX)     FA41990
      IF (ZPTH(IP).EQ.ZOUT(IOUTMX)) GO TO 150                            FA42000
      GO TO 110                                                          FA42010
  150 CONTINUE                                                           FA42020
      IPMAX = IP                                                         FA42030
C                                                                        FA42040
      RETURN                                                             FA42050
C                                                                        FA42060
  900 FORMAT ('0FROM AMERGE- ATMOSPHERIC PROFILE IN ZMDL DOES NOT',      FA42150
     *        ' EXTEND UP TO HMIN = ',E12.5)                             FA42160
  905 FORMAT ('0FROM AMERGE- MERGING THE ATMOSPHERIC PROFILE AND THE ',  FA42170
     *        'LBLRTM BOUNDARIES INTO ZPTH(IPDIM) EXCEEDS THE ',         FA42180
     *        'DIMENSION IPDIM = ',I5)                                   FA42190
C                                                                        FA42200
      END                                                                FA42210
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE ALAYER (J,SINAI,COSAI,CPATH,SH,GAMMA,IAMT,S,BEND)       FA42220
C                                                                        FA42230
C     -------------------------------------------------------------             
C     This routine was modified for LBLRTM to reflect changes                   
C     implemented in MODTRAN to solve problems with inconsistent                
C     path parameters.                                                          
C     It was also modified to eliminate GOTO statements in order to             
C     make the program easier to understand.                                    
C     These changes were obtained from H. Snell (March, 1996).                  
C     -------------------------------------------------------------             
C                                                                               
C     *****************************************************************  FA42240
C     THIS SUBROUTINE TRACES THE OPTICAL RAY THROUGH ONE LAYER FROM      FA42250
C     Z1 TO Z2 AND IF IAMT.NE.2 CALCULATES THE INTEGRATED ABSORBER       FA42260
C     AMOUNTS FOR THE LAYER. SINAI IS THE SIN OF THE INITIAL INCIDENCE   FA42270
C     ANGLE (= 180 - ZENITH ANGLE). COSAI IS CARRIED SEPERATELY TO       FA42280
C     AVOID A PRECISION PROBLEM NEAR SINAI = 1. CPATH IS THE CONSTANT    FA42290
C     OF REFRACTION FOR THE PATH = INDEX*RADIUS*SINAI, SH AND GAMMA ARE  FA42300
C     THE SCALE HEIGHT AND THE AMOUNT AT THE GROUND FOR THE REFRACTIVIT  FA42310
C     (= 1-INDEX OF REFRACTION), S IS THE REFRACTED PATH LENGTH THROUGH  FA42320
C     THE LAYER, BETA IS THE EARTH CENTERED ANGLE, AND BEND IS THE       FA42330
C     BENDING THROUGH THE LAYER. IAMT CONTROLS WHETHER AMOUNTS ARE       FA42340
C     CALCULATED OR NOT.                                                 FA42350
C     *****************************************************************  FA42360
C                                                                        FA42370
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA42380
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA42390
C                                                                        FA42400
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA42410
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA42420
     *              NLTEFL,LNFIL4,LNGTH4                                 FA42430
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA42450
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA42460
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA42470
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA42480
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA42490
C                                                                        FA42500
      CHARACTER*8      HMOD                                             &FA42510
C                                                                        FA42520
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                        FA42570
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FA42580
      DIMENSION HDEN(MXMOL),DENA(MXMOL),DENB(MXMOL)                      FA42590
C                                                                               
      REAL*8           S,BEND,DS,DBEND,W1,W2,W3,DSDX1,DSDX2,DSDX3,              
     *       DBNDX1,DBNDX2,DBNDX3,R1,R2,R3,X1,X2,X3,RATIO1,RATIO2,              
     *       RATIO3,SINAI1,SINAI2,SINAI3,COSAI1,COSAI2,COSAI3,Y1,Y3,            
     *       CPATH,DX,DH,SINAI,COSAI,D31,D32,D21,DHMIN,                         
     *       SH,GAMMA,ANDEXD,RADRFD                                             
C                                                                        FA42600
      DATA EPSILN / 1.0E-5 /                                             FA42610
C                                                                        FA42620
C     INITIALIZE VARIABLES FOR THE CALCULATION OF THE PATH               FA42630
C                                                                        FA42640
      N = 0                                                              FA42650
      Z1 = ZPTH(J)                                                       FA42660
      Z2 = ZPTH(J+1)                                                     FA42670
      H1 = Z1                                                            FA42680
      R1 = RE+H1                                                         FA42690
      DHMIN = DELTAS**2/(2.0*R1)                                         FA42700
      SINAI1 = SINAI                                                     FA42710
      COSAI1 = COSAI                                                     FA42720
      IF ((1.0-SINAI).LT.EPSILN)                                         FA42730
     *     Y1 = COSAI1**2/2.0+COSAI1**4/8.0+COSAI1**6*3.0/48.0           FA42740
      Y3 = 0.0                                                           FA42750
      X1 = -R1*COSAI1                                                    FA42760
      RATIO1 = R1/RADRFD(H1,SH,GAMMA)                                    FA42770
      DSDX1 = 1.0/(1.0-RATIO1*SINAI1**2)                                 FA42780
      DBNDX1 = DSDX1*SINAI1*RATIO1/R1                                    FA42790
      S = 0.0                                                            FA42800
      BEND = 0.0                                                         FA42810
      IF (IAMT.NE.2) THEN                                                       
C                                                                        FA42830
C         Initialize the variables for the calculation of the            FA42840
C         absorber amounts                                               FA42850
C                                                                        FA42860
          PA = PP(J)                                                     FA42870
          PB = PP(J+1)                                                   FA42880
          IF (PB.EQ.PA) THEN                                                    
             WRITE(*,*) PB                                                      
             STOP 'LBLATM: PRESSURES IN ADJOINING LAYERS MUST DIFFER'           
          ENDIF                                                                 
          TA = TP(J)                                                     FA42890
          TB = TP(J+1)                                                   FA42900
          RHOA = PA/(GCAIR*TA)                                           FA42910
          RHOB = PB/(GCAIR*TB)                                           FA42920
          DZ = ZPTH(J+1)-ZPTH(J)                                         FA42930
          HP = -DZ/ LOG(PB/PA)                                           FA42940
          IF (ABS(RHOB/RHOA-1.0).GE.EPSILN) THEN                         FA42950
              HRHO = -DZ/ LOG(RHOB/RHOA)                                 FA42960
          ELSE                                                           FA42970
              HRHO = 1.0E30                                              FA42980
          ENDIF                                                          FA42990
          DO 40 K = 1, NMOL                                              FA43000
              DENA(K) = DENP(K,J)                                        FA43010
              DENB(K) = DENP(K,J+1)                                      FA43020
              IF ((DENA(K).EQ.0.0.OR.DENB(K).EQ.0.0).OR.                 FA43030
     *            (ABS(1.0-DENA(K)/DENB(K)).LE.EPSILN)) THEN             FA43040
C                                                                               
C                 Use linear interpolation                                      
C                                                                               
                  HDEN(K) = 0.0                                                 
              ELSE                                                              
C                                                                        FA43050
C                 Use exponential interpolation                          FA43060
C                                                                        FA43070
                  HDEN(K) = -DZ/ LOG(DENB(K)/DENA(K))                    FA43080
              ENDIF                                                             
  40      CONTINUE                                                       FA43140
      ENDIF                                                                     
C                                                                        FA43160
C     LOOP THROUGH PATH                                                  FA43170
C     INTEGRATE PATH QUANTITIES USING QUADRATIC INTEGRATION WITH         FA43180
C     UNEQUALLY SPACED POINTS                                            FA43190
C                                                                        FA43200
   60 CONTINUE                                                           FA43210
      N = N+1                                                            FA43220
      DH = -DELTAS*COSAI1                                                FA43230
      DH = MAX(DH,DHMIN)                                                 FA43240
      H3 = H1+DH                                                         FA43250
      IF (H3.GT.Z2) H3 = Z2                                              FA43260
      DH = H3-H1                                                         FA43270
      R3 = RE+H3                                                         FA43280
      H2 = H1+DH/2.0                                                     FA43290
      R2 = RE+H2                                                         FA43300
      SINAI2 = CPATH/(ANDEXD(H2,SH,GAMMA)*R2)                            FA43310
      SINAI3 = CPATH/(ANDEXD(H3,SH,GAMMA)*R3)                            FA43320
      RATIO2 = R2/RADRFD(H2,SH,GAMMA)                                    FA43330
      RATIO3 = R3/RADRFD(H3,SH,GAMMA)                                    FA43340
      IF ((1.0-SINAI2).LE.EPSILN) THEN                                          
C                                                                        FA43360
C        Near a tangent height, COSAI = -SQRT(1-SINAI**2) loses          FA43370
C        precision. use the following algorithm to get COSAI.            FA43380
C                                                                        FA43390
         Y3 = Y1+(SINAI1*(1.0-RATIO1)/R1+4.0*SINAI2*(1.0-RATIO2)/R2+     FA43400
     *        SINAI3*(1.0-RATIO3)/R3)*DH/6.0                             FA43410
         COSAI3 = -SQRT(2.0*Y3-Y3**2)                                    FA43420
         X3 = -R3*COSAI3                                                 FA43430
         DX = X3-X1                                                      FA43440
         W1 = 0.5*DX                                                     FA43450
         W2 = 0.0                                                        FA43460
         W3 = 0.5*DX                                                     FA43470
      ELSE                                                               FA43480
         COSAI2 = -SQRT(1.0-SINAI2**2)                                   FA43510
         COSAI3 = -SQRT(1.0-SINAI3**2)                                   FA43520
         X2 = -R2*COSAI2                                                 FA43530
         X3 = -R3*COSAI3                                                 FA43540
C                                                                        FA43550
C        Calculate weights                                               FA43560
C                                                                        FA43570
         D31 = X3-X1                                                     FA43580
         D32 = X3-X2                                                     FA43590
         D21 = X2-X1                                                     FA43600
         IF (D32.EQ.0.0.OR.D21.EQ.0.0) THEN                              FA43610
            W1 = 0.5*D31                                                 FA43620
            W2 = 0.0                                                     FA43630
            W3 = 0.5*D31                                                 FA43640
         ELSE                                                            FA43650
            W1 = (2.0-D32/D21)*D31/6.0                                   FA43660
            W2 = D31**3/(D32*D21*6.0)                                    FA43670
            W3 = (2.0-D21/D32)*D31/6.0                                   FA43680
         ENDIF                                                           FA43700
      ENDIF                                                              FA43710
      DSDX2 = 1.0/(1.0-RATIO2*SINAI2**2)                                 FA43720
      DSDX3 = 1.0/(1.0-RATIO3*SINAI3**2)                                 FA43730
      DBNDX2 = DSDX2*SINAI2*RATIO2/R2                                    FA43740
      DBNDX3 = DSDX3*SINAI3*RATIO3/R3                                    FA43750
C                                                                        FA43760
C     INTEGRATE                                                          FA43770
C                                                                        FA43780
      DS = W1*DSDX1+W2*DSDX2+W3*DSDX3                                    FA43790
      S = S+DS                                                           FA43800
      DBEND = W1*DBNDX1+W2*DBNDX2+W3*DBNDX3                              FA43810
      BEND = BEND+DBEND                                                  FA43820
      IF (IAMT.NE.2) THEN                                                FA43830
C                                                                        FA43840
C         Calculate amounts                                              FA43850
C                                                                        FA43860
         DSDZ = DS/DH                                                    FA43870
         PB = PA*EXP(-DH/HP)                                             FA43880
         RHOB = RHOA*EXP(-DH/HRHO)                                       FA43890
         IF ((DH/HRHO).GE.EPSILN) THEN                                          
            PPSUM(J) = PPSUM(J)+DSDZ*(HP/(1.0+HP/HRHO))                  FA43900
     *                 *(PA*RHOA-PB*RHOB)                                FA43910
            TPSUM(J) = TPSUM(J)+DSDZ*HP*(PA-PB)/GCAIR                    FA43920
            RHOPSM(J) = RHOPSM(J)+DSDZ*HRHO*(RHOA-RHOB)                  FA43930
         ELSE                                                            FA43940
            PPSUM(J) = PPSUM(J)+0.5*DS*(PA*RHOA+PB*RHOB)                 FA43960
            TPSUM(J) = TPSUM(J)+0.5*DS*(PA+PB)/GCAIR                     FA43970
            RHOPSM(J) = RHOPSM(J)+0.5*DS*(RHOA+RHOB)                     FA43980
         ENDIF                                                           FA43990
         DO 130 K = 1, NMOL                                              FA44000
            IF ((HDEN(K).EQ.0.0).OR.                                     FA44010
     *           (ABS(DH/HDEN(K)).LT.EPSILN)) THEN                       FA44020
C                                                                        FA44130
C                 Linear interpolation                                   FA44140
C                 1.0E05 factor converts units km to cm                         
C                                                                        FA44150
               DENB(K)=DENP(K,J)+(DENP(K,J+1)-DENP(K,J))*(H3-Z1)/DZ      FA44160
               AMTP(K,J) = AMTP(K,J)+0.5*(DENA(K)+DENB(K))*DS*1.0E5      FA44170
            ELSE                                                                
C                                                                               
C                 Exponential interpolation                                     
C                                                                               
               DENB(K) = DENP(K,J)*EXP(-(H3-Z1)/HDEN(K))                        
               AMTP(K,J) = AMTP(K,J)+DSDZ*HDEN(K)                               
     *              *(DENA(K)-DENB(K))*1.0E5                                    
            ENDIF                                                               
 130     CONTINUE                                                        FA44180
         PA = PB                                                         FA44190
         RHOA = RHOB                                                     FA44200
         DO 140 K = 1, NMOL                                              FA44210
            DENA(K) = DENB(K)                                            FA44220
 140     CONTINUE                                                        FA44230
      ENDIF                                                              FA44240
C                                                                               
      IF (H3.LT.Z2) THEN                                                 FA44250
         H1 = H3                                                         FA44260
         R1 = R3                                                         FA44270
         SINAI1 = SINAI3                                                 FA44280
         RATIO1 = RATIO3                                                 FA44290
         Y1 = Y3                                                         FA44300
         COSAI1 = COSAI3                                                 FA44310
         X1 = X3                                                         FA44320
         DSDX1 = DSDX3                                                   FA44330
         DBNDX1 = DBNDX3                                                 FA44340
      ELSE                                                                      
         SINAI = SINAI3                                                  FA44370
         COSAI = COSAI3                                                  FA44380
         SP(J) = S                                                       FA44390
         RETURN                                                          FA44410
      ENDIF                                                                     
C                                                                               
      GO TO 60                                                           FA44350
C                                                                        FA44400
      END                                                                FA44430
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE AUTLAY (HMIN,HMAX,XVBAR,AVTRAT,TDIFF1,TDIFF2,ALTD1,     FA44440
     *                   ALTD2,IERROR)                                   FA44450
C                                                                        FA44460
C     *****************************************************************  FA44470
C     THIS SUBROUTINE AUTOMATICALLY SELECTS A SET OF LBLRTM BOUNDARY     FA44480
C     LEVELS WHICH SATISFY THE FOLLOWING TWO TESTS:                      FA44490
C          1. THE RATIO OF THE VOIGT HALFWIDTHS BETWEEN BOUNDARIES       FA44500
C             IS LESS THAN OR EQUAL TO AVTRAT, AND                       FA44510
C          2. THE TEMPERATURE DIFFERENCE BETWEEN BOUNDARIES IS           FA44520
C             LESS THAN OR EQUAL TO TDIFF                                FA44530
C     TDIFF VARIES FROM TDIFF1 AT HMIN TO TDIFF2 AT HMAX,                FA44540
C     WITH EXPONENTIAL INTERPOLATION BETWEEN                             FA44550
C     THESE BOUNDARIES ARE ROUNDED DOWN TO THE NEAREST TENTH KM          FA44560
C     NOTE THAT THESE TESTS APPLY TO THE LAYER BOUNDARIES                FA44570
C     NOT TO THE AVERAGE VALUES FROM ONE LAYER TO THE NEXT.              FA44580
C     *****************************************************************  FA44590
C                                                                        FA44600
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA44610
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA44620
C                                                                        FA44630
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA44640
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA44650
     *              NLTEFL,LNFIL4,LNGTH4                                 FA44660
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA44680
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA44690
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA44720
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA44730
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA44740
C                                                                        FA44750
      CHARACTER*8      HMOD                                             &FA44760
C                                                                        FA44770
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                        FA44820
      COMMON /BNDRY/ ZBND(MXFSC),PBND(MXFSC),TBND(MXFSC),ALORNZ(MXFSC),  FA44830
     *               ADOPP(MXFSC),AVOIGT(MXFSC)                          FA44840
      DIMENSION AVTM(MXZMD)                                              FA44850
C                                                                        FA44860
C     FUNCTION ZROUND ROUNDS THE ALTITUDE Z DOWN TO THE                  FA44870
C     NEAREST TENTH KM                                                   FA44880
C                                                                        FA44890
      ZROUND(ZX) = 0.1* REAL( INT(10.0*ZX))                              FA44900
      HMIN = MAX(HMIN,ZMDL(1))                                           FA44910
C                                                                        FA44920
      DO 10 IM = 2, IMMAX                                                FA44930
         IHMIN = IM                                                      FA44940
         IF (ZMDL(IM).GT.HMIN) GO TO 20                                  FA44950
   10 CONTINUE                                                           FA44960
   20 CONTINUE                                                           FA44970
      HTOP = HMAX                                                        FA44980
      HTOP = MIN(HTOP,ZMAX)                                              FA44990
      IM = IHMIN-1                                                       FA45000
      ZZ = ZMDL(IM)                                                      FA45010
      CALL HALFWD (ZZ,XVBAR,P,T,AL,AD,AVTM(IM))                          FA45020
      IB = 1                                                             FA45030
      ZBND(IB) = HMIN                                                    FA45040
      IM = IHMIN                                                         FA45050
      CALL HALFWD (ZBND(IB),XVBAR,PBND(IB),TBND(IB),ALORNZ(IB),          FA45060
     *             ADOPP(IB),AVOIGT(IB))                                 FA45070
C                                                                        FA45080
C     BEGIN IM LOOP                                                      FA45090
C                                                                        FA45100
   30 CONTINUE                                                           FA45110
      IB = IB+1                                                          FA45120
      IF (IB.GT.IBDIM) GO TO 90                                          FA45130
      IBM1 = IB-1                                                        FA45140
      TMIN = TBND(IBM1)                                                  FA45150
      TMAX = TBND(IBM1)                                                  FA45160
      IND = 0                                                            FA45170
C                                                                        FA45180
C     BEGIN IB LOOP                                                      FA45190
C                                                                        FA45200
   40 CONTINUE                                                           FA45210
      IPASS = 0                                                          FA45220
      ZBND(IB) = ZMDL(IM)                                                FA45230
      ZBNDTI = ZMDL(IM)                                                  FA45240
      IF (ZBND(IB).GE.HTOP) ZBND(IB) = HTOP                              FA45250
      CALL HALFWD (ZBND(IB),XVBAR,PBND(IB),TBND(IB),ALORNZ(IB),          FA45260
     *             ADOPP(IB),AVOIGT(IB))                                 FA45270
      AVTM(IM) = AVOIGT(IB)                                              FA45280
C                                                                        FA45290
C     TEST THE RATIO OF THE VOIGT WIDTHS AGAINST AVTRAT                  FA45300
C                                                                        FA45310
      IF ((AVOIGT(IB-1)/AVOIGT(IB)).LT.AVTRAT) GO TO 50                  FA45320
C                                                                        FA45330
C     ZMDL(IM) FAILS THE HALFWIDTH RATIO TEST                            FA45340
C                                                                        FA45350
      IPASS = 1                                                          FA45360
      AVOIGT(IB) = AVOIGT(IB-1)/AVTRAT                                   FA45370
      X = AVTM(IM)/AVTM(IM-1)                                            FA45380
      ALOGX = 1.-X                                                       FA45390
      IF (ABS(ALOGX).LT.0.001) THEN                                      FA45400
         ZBND(IB) = (ZMDL(IM)+ZMDL(IM-1))/2.                             FA45410
         GO TO 50                                                        FA45420
      ELSE                                                               FA45430
         ALOGX =  LOG(X)                                                 FA45440
      ENDIF                                                              FA45450
      Y = AVOIGT(IB)/AVTM(IM-1)                                          FA45460
      ALOGY = 1.-Y                                                       FA45470
      IF (ABS(ALOGY).GT.0.001) ALOGY =  LOG(Y)                           FA45480
      ZBND(IB) = ZMDL(IM-1)+(ZMDL(IM)-ZMDL(IM-1))*ALOGY/ALOGX            FA45490
   50 CONTINUE                                                           FA45500
C                                                                        FA45510
C     TEST THE TEMPERATURE DIFFERENCE AGAINST TDIFF                      FA45520
C                                                                        FA45530
      FAC = (ZBND(IB-1)-ALTD1)/(ALTD2-ALTD1)                             FA45540
      CALL EXPINT (TDIFF,TDIFF1,TDIFF2,FAC)                              FA45550
      IF (TM(IM).GT.TMAX) THEN                                           FA45560
         IND = 1                                                         FA45570
         TMAX = TM(IM)                                                   FA45580
      ENDIF                                                              FA45590
      IF (TM(IM).LT.TMIN) THEN                                           FA45600
         IND = 2                                                         FA45610
         TMIN = TM(IM)                                                   FA45620
      ENDIF                                                              FA45630
      IF (TMAX-TMIN.LE.TDIFF) GO TO 60                                   FA45640
      IF (IND.EQ.1) TBND(IB) = TMIN+TDIFF                                FA45650
      IF (IND.EQ.2) TBND(IB) = TMAX-TDIFF                                FA45660
C                                                                        FA45670
C     ZBND(IB) FAILS THE TEMPERATURE DIFFERENCE TEST                     FA45680
C                                                                        FA45690
      IPASS = 2                                                          FA45700
      IF (ABS(TM(IM)-TM(IM-1)).LT.0.0001) THEN                           FA45710
         ZBNDTI = (ZMDL(IM)+ZMDL(IM-1))/2.                               FA45720
      ELSE                                                               FA45730
         ZBNDTI = ZMDL(IM-1)+(ZMDL(IM)-ZMDL(IM-1))*                      FA45740
     *            (TBND(IB)-TM(IM-1))/(TM(IM)-TM(IM-1))                  FA45750
      ENDIF                                                              FA45760
   60 CONTINUE                                                           FA45770
      IF (ZBNDTI.LT.ZBND(IB)) ZBND(IB) = ZBNDTI                          FA45780
      IF (ZBND(IB).GE.HTOP) THEN                                         FA45790
         ZBND(IB) = HTOP                                                 FA45800
         IF (ZBND(IB)-ZBND(IB-1).LE.0.1) THEN                            FA45810
            IB = IB-1                                                    FA45820
            ZBND(IB) = HTOP                                              FA45830
            CALL HALFWD (ZBND(IB),XVBAR,PBND(IB),TBND(IB),ALORNZ(IB),    FA45840
     *                   ADOPP(IB),AVOIGT(IB))                           FA45850
         ENDIF                                                           FA45860
         GO TO 80                                                        FA45870
      ENDIF                                                              FA45880
      IF (IPASS.NE.0) GO TO 70                                           FA45890
C                                                                        FA45900
C     BOTH HALFWIDTH AND TEMPERATURE TEST PASS FOR ZBND(IB) = ZMDL(IM),  FA45910
C     NOW TRY ZBND(IB) = ZMDL(IM+1)                                      FA45920
C                                                                        FA45930
      IM = IM+1                                                          FA45940
      GO TO 40                                                           FA45950
   70 CONTINUE                                                           FA45960
C                                                                        FA45970
C     ONE OF THE TESTS FAILED AND A NEW BOUNDRY ZBND WAS PRODUCED        FA45980
C                                                                        FA45990
      ZBND(IB) = ZROUND(ZBND(IB))                                        FA46000
      CALL HALFWD (ZBND(IB),XVBAR,PBND(IB),TBND(IB),ALORNZ(IB),          FA46010
     *             ADOPP(IB),AVOIGT(IB))                                 FA46020
      GO TO 30                                                           FA46030
   80 CONTINUE                                                           FA46040
      IBMAX = IB                                                         FA46050
      WRITE (IPR,900) AVTRAT,TDIFF1,HMIN,TDIFF2,HMAX                     FA46060
C                                                                        FA46070
      RETURN                                                             FA46080
C                                                                        FA46090
   90 CONTINUE                                                           FA46100
      WRITE (IPR,905) IBDIM                                              FA46110
      IBMAX = IBDIM                                                      FA46120
      IERROR = 5                                                         FA46130
C                                                                        FA46140
      RETURN                                                             FA46150
C                                                                        FA46160
  900 FORMAT (///,                                                       FA46170
     *        ' LBLRTM LAYER BOUNDARIES PRODUCED BY THE AUTOMATIC ',     FA46180
     *        'LAYERING ROUTINE AUTLAY',/,' THE USER SHOULD EXAMINE ',   FA46190
     *        'THESE BOUNDARIES AND MODIFY THEM IF APPROPRIATE',/,       FA46200
     *        ' THE FOLLOWING PARAMETERS ARE USED:',//,10X,              FA46210
     *        'AVTRAT    = ',F8.2,'       = MAX RATIO OF VOIGT WIDTHS',  FA46220
     *        /,10X,'TDIFF1    = ',F8.2,'       = MAX TEMP DIFF AT ',    FA46230
     *        F4.0,' KM',/10X,'TDIFF2    = ',F8.2,                       FA46240
     *        '       = MAX TEMP DIFF AT ',F4.0,' KM')                   FA46250
  905 FORMAT (///,' ERROR IN AUTLAY:',/,5X,'THE NUMBER OF ',             FA46260
     *        'GENERATED LAYER BOUNDARIES EXCEEDS THE DIMENSION IBDIM',  FA46270
     *        ' OF THE ARRAY ZBND.  IBDIM = ',I5,/,5X,'PROBABLE CAUSE',  FA46280
     *        ': EITHER AVTRAT AND/OF TDIFF ARE TOO SMALL',/,5X,         FA46290
     *        'THE GENERATED LAYERS FOLLOW')                             FA46300
C                                                                        FA46310
      END                                                                FA46320
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE HALFWD_P (Z,XVBAR,P,T,ALORNZ,ADOPP,AVOIGT)              FA46330
C                                                                        FA46340
C     *****************************************************************  FA46350
C     GIVEN AN PRESSURE AND TEMP. AND AVERAGE WAVENUMBER VBAR, THIS      FA46360
C     SUBROUTINE                                                         FA46370
C     CALCULATES THE LORENTZ, THE DOPPLER, AND THE VOIGT HALFWIDTHS      FA46380
C     (AT HALFHEIGHT) ALORNZ, ADOPP, AND AVOIGT RESPECTIVELY FOR         FA46390
C     THE ALTITUDE Z                                                     FA46400
C     AN AVERAGE LORENTZ WIDTH ALZERO AND AN AVERAGE MOLECULAR           FA46410
C     WEIGHT AVMWT ARE ASSUMED                                           FA46420
C     *****************************************************************  FA46430
C                                                                        FA46440
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA46450
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA46460
C                                                                        FA46470
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA46480
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA46490
     *              NLTEFL,LNFIL4,LNGTH4                                 FA46500
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA46520
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA46530
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA46560
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA46570
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA46580
C                                                                        FA46590
      CHARACTER*8      HMOD                                             &FA46600
C                                                                        FA46610
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                        FA46660
C     FUNCTIONS                                                          FA46670
C     ALZERO IS AT 1013.25 MB AND 296.0 K                                FA46680
C                                                                        FA46690
      ALPHAL(P,T) = ALZERO*(P/PZERO)*SQRT(296.0/T)                       FA46700
      ALPHAD(T,V) = ADCON*V*SQRT(T/AVMWT)                                FA46710
      ALPHAV(AL,AD) = 0.5*(AL+SQRT(AL**2+4.0*AD**2))                     FA46720
C                                                                        FA46730
      ALORNZ = ALPHAL(P,T)                                               FA46830
      ADOPP = ALPHAD(T,XVBAR)                                            FA46840
      AVOIGT = ALPHAV(ALORNZ,ADOPP)                                      FA46850
C                                                                        FA46860
      RETURN                                                             FA46870
C                                                                        FA46880
      END                                                                FA46890
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE HALFWD (Z,XVBAR,P,T,ALORNZ,ADOPP,AVOIGT)                FA46330
C                                                                        FA46340
C     *****************************************************************  FA46350
C     GIVEN AN ALTITUDE Z AND AN AVERAGE WAVENUMBER VBAR, THIS           FA46360
C     SUBROUTINE INTERPOLATES P AND T FROM THE PROFILE IN ZMDL  AND      FA46370
C     CALCULATES THE LORENTZ, THE DOPPLER, AND THE VOIGT HALFWIDTHS      FA46380
C     (AT HALFHEIGHT) ALORNZ, ADOPP, AND AVOIGT RESPECTIVELY FOR         FA46390
C     THE ALTITUDE Z                                                     FA46400
C     AN AVERAGE LORENTZ WIDTH ALZERO AND AN AVERAGE MOLECULAR           FA46410
C     WEIGHT AVMWT ARE ASSUMED                                           FA46420
C     *****************************************************************  FA46430
C                                                                        FA46440
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA46450
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA46460
C                                                                        FA46470
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA46480
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA46490
     *              NLTEFL,LNFIL4,LNGTH4                                 FA46500
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA46520
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA46530
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA46560
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA46570
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA46580
C                                                                        FA46590
      CHARACTER*8      HMOD                                             &FA46600
C                                                                        FA46610
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                        FA46660
C     FUNCTIONS                                                          FA46670
C     ALZERO IS AT 1013.25 MB AND 296.0 K                                FA46680
C                                                                        FA46690
      ALPHAL(P,T) = ALZERO*(P/PZERO)*SQRT(296.0/T)                       FA46700
      ALPHAD(T,V) = ADCON*V*SQRT(T/AVMWT)                                FA46710
      ALPHAV(AL,AD) = 0.5*(AL+SQRT(AL**2+4.0*AD**2))                     FA46720
C                                                                        FA46730
      DO 10 I2 = 2, IMMAX                                                FA46740
         IM = I2                                                         FA46750
         IF (ZMDL(IM).GE.Z) GO TO 20                                     FA46760
   10 CONTINUE                                                           FA46770
      IM = IMMAX                                                         FA46780
   20 CONTINUE                                                           FA46790
      FAC = (Z-ZMDL(IM-1))/(ZMDL(IM)-ZMDL(IM-1))                         FA46800
      CALL EXPINT (P,PM(IM-1),PM(IM),FAC)                                FA46810
      T = TM(IM-1)+(TM(IM)-TM(IM-1))*FAC                                 FA46820
      ALORNZ = ALPHAL(P,T)                                               FA46830
      ADOPP = ALPHAD(T,XVBAR)                                            FA46840
      AVOIGT = ALPHAV(ALORNZ,ADOPP)                                      FA46850
C                                                                        FA46860
      RETURN                                                             FA46870
C                                                                        FA46880
      END                                                                FA46890
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE FPACK (H1,H2,HMID,LEN,IEMIT,NOZERO)                     FA46900
C                                                                        FA46910
C     *****************************************************************  FA46920
C     FPACK TAKES THE AMOUNTS STORED IN THE LAYERS DEFINED BY ZPTH AND   FA46930
C     PACKS THEM INTO THE LAYERS DEFINED BY ZOUT.  IT ALSO ZEROS OUT     FA46940
C     LAYER AMOUNTS IF THE AMOUNT FOR THAT LAYER AND ABOVE IS LESS       FA46950
C     THAN 0.1 PERCENT OF THE TOTAL FOR THAT MOLECULE, UNLESS THE        FA46960
C     NOZERO OPTION IS SELECTED.                                         FA46970
C     *****************************************************************  FA46980
C                                                                        FA46990
      IMPLICIT REAL*8           (V)                                             
c                                                                               
      CHARACTER*8      XID,       HMOLID,      YID                      &FA00640
      Real*8               SECANT,       XALTZ                                  
C                                                                               
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FA47000
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FA47010
C                                                                        FA47020
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA47030
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA47040
     *              NLTEFL,LNFIL4,LNGTH4                                 FA47050
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA47070
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FA47080
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FA47110
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FA47120
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FA47130
C                                                                        FA47140
      CHARACTER*8      HMOD                                             &FA47150
C                                                                        FA47160
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                        FA47210
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FA47220
      COMMON /PROFILE/ NLAYRS,PBAR(MXLAY),TBAR(MXLAY),
     *                 PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /MANE1/ P0,TEMP0,DVXM,H2OSLF,WTOT,ALBAR,ADBAR,AVBAR,
     *                 AVFIX,LAYRFX,SECNT0,SAMPLE,DVSET,ALFAL0,AVMASS,
     *                 DPTMIN,DPTFAC,ALTAV,AVTRAT,TDIFF1,TDIFF2,ALTD1,
     *                 ALTD2,ANGLE,IANT,LTGNT,LH1,LH2,IPFLAG,PLAY,TLAY,
     *                 EXTID(10)
      COMMON /SPECIES/ COLDRY(MXLAY),AMOUNT(MXMOL,MXLAY),WN2L(MXLAY),
     *                 CDUM(MXLAY),NMOLEC
      COMMON /PATHD1/ DVL(MXLAY),WTOTL(MXLAY),
     *                 ALBL(MXLAY),ADBL(MXLAY),AVBL(MXLAY),
     *                 H2OSL(MXLAY),IPATH(MXLAY),ITYL(MXLAY),
     *                 SECNTA(MXLAY),HT1,HT2,ALTZ(0:MXLAY)
      COMMON /ZOUTP/ ZOUT(MXLAY),SOUT(MXLAY),RHOSUM(MXLAY),              FA47280
     *               AMTTOT(MXMOL),AMTCUM(MXMOL),ISKIP(MXMOL)            FA47290
      COMMON /FILHDR/ XID(10),SECANT,PAVE,TAVE,HMOLID(60),XALTZ(4),             
     *                WK(60),PZL,PZU,TZL,TZU,WN2   ,DV ,V1 ,V2 ,TBOUND,         
     *                EMISIV,FSCDID(17),NDUM,LAYER ,YI1,YID(10),LSTWDF          
C                                                                        FA47300
      character*4 ht1,ht2                                                       
                                                                                
      I2 = IPMAX-1                                                       FA47310
      IOUT = 1                                                           FA47320
      PZ(0) = PP(1)                                                      FA47330
      TZ(0) = TP(1)                                                      FA47340
C                                                                               
C     If entry in TAPE5 for TBOUND < 0, use TZ(O) as boundary                   
C     temperature                                                               
C                                                                               
C     Card removed in RRTM
C                                                                               
      DO 20 IP = 1, I2                                                   FA47350
         PBAR(IOUT) = PBAR(IOUT)+PPSUM(IP)                               FA47360
         TBAR(IOUT) = TBAR(IOUT)+TPSUM(IP)                               FA47370
         RHOSUM(IOUT) = RHOSUM(IOUT)+RHOPSM(IP)                          FA47380
         SOUT(IOUT) = SOUT(IOUT)+SP(IP)                                  FA47390
         DO 10 K = 1, NMOL                                               FA47400
            AMOUNT(K,IOUT) = AMOUNT(K,IOUT)+AMTP(K,IP)                   FA47410
   10    CONTINUE                                                        FA47420
         IF (ZPTH(IP+1).EQ.ZOUT(IOUT+1)) THEN                            FA47430
            PZ(IOUT) = PP(IP+1)                                          FA47440
            TZ(IOUT) = TP(IP+1)                                          FA47450
            IOUT = IOUT+1                                                FA47460
         ENDIF                                                           FA47470
   20 CONTINUE                                                           FA47480
      IF (IOUT.NE.IOUTMX) GO TO 110                                      FA47490
C                                                                        FA47500
C     CALCULATE THE DENSITY WEIGHTED PRESSURE AND TEMPERATURE AND        FA47510
C     ZERO OUT LAYER AMOUNTS AFTER 99.9 PERCENT OF THE TOTAL             FA47520
C                                                                        FA47530
      iskip(7) = 0                                                              
c                                                                               
      DO 30 K = 1, NMOL                                                  FA47540
         AMTCUM(K) = 0.0                                                 FA47550
         ISKIP(K) = 0                                                    FA47560
         IF (AMTTOT(K).EQ.0.0) ISKIP(K) = 1                              FA47570
   30 CONTINUE                                                           FA47580
      L2 = IOUTMX-1                                                      FA47590
      LMAX = L2                                                          FA47600
      DO 90 L = 1, L2                                                    FA47610
         PBAR(L) = PBAR(L)/RHOSUM(L)                                     FA47620
         TBAR(L) = TBAR(L)/RHOSUM(L)                                     FA47630
C                                                                        FA47640
C     ADJUST RHOSUM FOR THE PATH LENGTH IN CM NOT KM                     FA47650
C                                                                        FA47660
         RHOSUM(L) = RHOSUM(L)*1.0E+5                                    FA47670
C                                                                        FA47680
         SUMAMT = 0.                                                     FA47690
         DO 40 K = 1, NMOL                                               FA47700
            SUMAMT = SUMAMT+AMOUNT(K,L)                                  FA47710
   40    CONTINUE                                                        FA47720
         WN2L(L) = RHOSUM(L)-SUMAMT                                      FA47730
C                                                                        FA47740
C     CALCULATE 'EFFECTIVE SECANT' SECNTA                                FA47750
C                                                                        FA47760
         SECNTA(L) = SOUT(L)/(ZOUT(L+1)-ZOUT(L))                         FA47770
         IF (L.EQ.1) ALTZ(0) = ZOUT(1)                                   FA47780
         ALTZ(L) = ZOUT(L+1)                                             FA47790
C                                                                        FA47800
C     SET  IPATH                                                         FA47810
C                                                                        FA47820
         IF (LEN.EQ.1) GO TO 50                                          FA47830
         IF (H1.LT.H2) IPATH(L) = 3                                      FA47840
         IF (H1.GT.H2) IPATH(L) = 1                                      FA47850
         GO TO 60                                                        FA47860
   50    CONTINUE                                                        FA47870
         IF (ZOUT(L).LT.HMID) IPATH(L) = 2                               FA47880
         IF (ZOUT(L).GE.HMID.AND.H1.GT.H2) IPATH(L) = 1                  FA47890
         IF (ZOUT(L).GE.HMID.AND.H1.LT.H2) IPATH(L) = 3                  FA47900
   60    CONTINUE                                                        FA47910
C                                                                        FA47920
C     TEST FOR ZEROING OF AMOUNTS                                        FA47930
C                                                                        FA47940
         ISKPT = 0                                                       FA47950
         nmol_max = nmol                                                        
         IF (ISKIP(7).EQ.1) nmol_max = nmol - 1                                 
         FAC = 1.0                                                       FA47970
         IF (IPATH(L).EQ.2) FAC = 2.0                                    FA47980
C                                                                        FA47990
         DO 80 K = 1, NMOL                                               FA48000
                                                                                
            IF (NOZERO.EQ.1) go to 70                                           
                                                                                
            IF (ISKIP(K).NE.1) THEN                                      FA48010
               IF (K.EQ.7 .OR. (IEMIT.EQ.1.AND.IPATH(L).NE.3)) GO TO 70         
               IF (((AMTTOT(K)-AMTCUM(K))/AMTTOT(K)).GT.0.001) GO TO 70  FA48040
            ENDIF                                                        FA48050
C                                                                        FA48060
C     ZERO OUT THIS AMOUNT                                               FA48070
C                                                                        FA48080
            ISKIP(K) = 1                                                 FA48090
            AMOUNT(K,L) = 0.0                                            FA48100
            ISKPT = ISKPT+1                                              FA48110
C                                                                        FA48120
C     IF ALL BUT O2 ARE ZEROED, ELIMINATE ALL HIGHER LAYERS              FA48130
C                                                                        FA48140
            IF (ISKPT.GE.(NMOL_max))  GO TO 100                                 
   70       CONTINUE                                                     FA48160
            AMTCUM(K) = AMTCUM(K)+FAC*AMOUNT(K,L)                        FA48170
   80    CONTINUE                                                        FA48180
         LMAX = L                                                        FA48190
   90 CONTINUE                                                           FA48200
  100 CONTINUE                                                           FA48210
      IOUTMX = LMAX+1                                                    FA48220
C                                                                        FA48230
      RETURN                                                             FA48240
C                                                                        FA48250
  110 WRITE (IPR,900) IOUT,IOUTMX                                        FA48260
C                                                                        FA48270
      STOP ' ERROR FPACK '                                               FA48280
C                                                                        FA48290
  900 FORMAT ('0FROM FPACK-  ERROR, IOUT = ',I5,'  DOES NOT MATCH ',     FA48300
     *        'IOUTMX = ',I5)                                            FA48310
C                                                                        FA48320
      END                                                                FA48330
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
C                                                                               
      SUBROUTINE FIXTYP(IEMIT,FRH2O,ALFCOR,OLDDV,L,CINP)                        
C                                                                               
C     *****************************************************************         
C     This subroutine calculates ITYL, the ITYPE (ratio of DV from              
C     one layer to the next) for each layer for output to TAPE7, if             
C     desired (IFXTYP = 1).                                                     
C     *****************************************************************         
C                                                                               
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                           
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)            
C                                                                               
      CHARACTER*3 CINP                                                          
C                                                                               
      COMMON /PROFILE/ NLAYRS,PBAR(MXLAY),TBAR(MXLAY),
     *                 PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /MANE1/ P0,TEMP0,DVXM,H2OSLF,WTOT,ALBAR,ADBAR,AVBAR,
     *                 AVFIX,LAYRFX,SECNT0,SAMPLE,DVSET,ALFAL0,AVMASS,
     *                 DPTMIN,DPTFAC,ALTAV,AVTRAT,TDIFF1,TDIFF2,ALTD1,
     *                 ALTD2,ANGLE,IANT,LTGNT,LH1,LH2,IPFLAG,PLAY,TLAY,
     *                 EXTID(10)
      COMMON /SPECIES/ COLDRY(MXLAY),AMOUNT(MXMOL,MXLAY),WN2L(MXLAY),
     *                 CDUM(MXLAY),NMOLEC
      COMMON /PATHD1/ DVL(MXLAY),WTOTL(MXLAY),
     *                 ALBL(MXLAY),ADBL(MXLAY),AVBL(MXLAY),
     *                 H2OSL(MXLAY),IPATH(MXLAY),ITYL(MXLAY),
     *                 SECNTA(MXLAY),HT1,HT2,ALTZ(0:MXLAY)
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
C                                                                               
      DATA I_2/2/                                                               
C                                                                               
      DV = 0.                                                                   
C                                                                               
C     Correct for water self broadening                                         
C                                                                               
      H2OSLF = (1.-FRH2O+5.*FRH2O)                                              
      ALBAR = ALZERO*ALFCOR*H2OSLF                                              
C                                                                               
      AVBAR = 0.5*(ALBAR+SQRT(ALBAR*ALBAR+4.*ADBAR*ADBAR))                      
C                                                                               
      DV = AVBAR/SAMPLE                                                         
C                                                                               
      TYPE = 0.                                                                 
      ITYPE = 99                                                                
C                                                                               
C     DV is assumed to be less than 1                                           
C     Set DV to 3 significant figures                                           
C                                                                               
      IF (L.EQ.1) THEN                                                          
         ISCAL =  LOG10(DV)-3.                                                  
         SCAL = 10.**ISCAL                                                      
         IDV = (DV/SCAL)+0.5                                                    
C                                                                               
C        Set IDV to be even                                                     
C                                                                               
         IF (MOD(IDV,I_2).GT.0) IDV = IDV+1                                     
         DV = SCAL* REAL(IDV)                                                   
      ELSE                                                                      
         TYPE = OLDDV/DV                                                        
         TYPMAX = 2.5                                                           
         IF (TYPE.GT.TYPMAX) THEN                                               
            IPROB = 1                                                           
            ISTOP = 1                                                           
         ELSEIF (TYPE.GE.1.2) THEN                                              
C                                                                               
C           TYPE is between 1.2 and TYPMAX                                      
C                                                                               
            DV = OLDDV                                                          
            ITYPE = 1./(TYPE-1.)+0.5                                            
            IF (ITYPE.EQ.3) ITYPE = 2                                           
            DV = OLDDV* REAL(ITYPE)/ REAL(ITYPE+1)                              
         ELSEIF (TYPE.GE.0.8) THEN                                              
C                                                                               
C           TYPE is between 0.8 and 1.2 (set to 1.0)                            
C                                                                               
            DV = OLDDV                                                          
            ITYPE = 0                                                           
         ELSE                                                                   
C                                                                               
C           TYPE is less than 0.8                                               
C                                                                               
            DV = OLDDV                                                          
            ITYPE = 0                                                           
            IF (IEMIT.NE.1) THEN                                                
               ITYPE = TYPE/(1.-TYPE)+0.5                                       
               DV = DV* REAL(ITYPE+1)/ REAL(ITYPE)                              
               ITYPE = -ITYPE                                                   
            ENDIF                                                               
         ENDIF                                                                  
      ENDIF                                                                     
C                                                                               
      OLDDV = DV                                                                
C                                                                               
      WRITE(CINP,900) ITYPE                                                     
C                                                                               
      RETURN                                                                    
C                                                                               
 900  FORMAT(I3)                                                                
C                                                                               
      END                                                                       
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE XAMNTS (XV1,XV2)                                        FX00010
C                                                                        FX00020
C     *****************************************************************  FX00030
C     THIS SUBROUTINE GENERATES THE ABSORBER AMOUNTS FOR THE SELECTED    FX00040
C     HEAVY MOLECULES FOR WHICH "CROSS-SECTION" SPECTRAL DATA IS         FX00050
C     AVAILABLE.  THE USER SELECTS THE DESIRED MOLECULES BY              FX00060
C     USING THE CHEMICAL FORMULA, E.G. "CF2CL2" OR BY AN ALIAS, IN       FX00070
C     THIS CASE, "F12".  THEREAFTER, THE MOLECULES ARE IDENTIFIED BY     FX00080
C     AN INDEX.  THE USER MAY EITHER SELECT A STANDARD PROFILE OR READ   FX00090
C     IN ONE.                                                            FX00100
C                                                                        FX00110
C                             A.E.R. INC.     (AUGUST 1990)              FX00120
C    *****************************************************************   FX00130
C                                                                        FX00140
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FX00150
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FX00160
C                                                                        FX00170
      IMPLICIT REAL*8           (V)                                     !FX00171
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FX00180
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FX00190
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FX00200
C                                                                        FX00210
      CHARACTER*8      HMOD                                             &FX00220
C                                                                        FX00230
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                        FX00280
C     IFIL CARRIES FILE INFORMATION                                      FX00290
C                                                                        FX00300
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FX00310
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FX00320
     *              NLTEFL,LNFIL4,LNGTH4                                 FX00330
C                                                                        FX00340
C     LAMCHN CARRIES HARDWARE SPECIFIC PARAMETERS                        FX00350
C                                                                        FX00360
      COMMON /LAMCHN/ ONEPL,ONEMI,EXPMIN,ARGMIN                          FX00370
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
      COMMON /ADRIVE/ LOWFLG,IREAD,MODEL,ITYPE,NOZERO,NOP,H1F,H2F,       FX00400
     *                ANGLEF,RANGEF,BETAF,LENF,AV1,AV2,RO,IPUNCH,XVBAR,  FX00410
     *                HMINF,PHIF,IERRF,HSPACE                            FX00420
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FX00440
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FX00450
      COMMON /MLATM/ ALT(MXZMD),PMDL(MXZMD,6),TMDL(MXZMD,6),             FX00460
     *               AMOL(MXZMD,8,6),ZST(MXZMD),PST(MXZMD),              FX00470
     *               TST(MXZMD),AMOLS(MXZMD,MXMOL)                       FX00480
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FX00490
C                                                                        FX00500
C     COMMON BLOCKS AND PARAMETERS FOR THE PROFILES AND DENSITIES        FX00510
C     FOR THE CROSS-SECTION MOLECULES.                                   FX00520
C     XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES         FX00530
C                                                                        FX00540
      CHARACTER*10 XSFILE,XSNAME,ALIAS                                   FX00550
      COMMON /XSECTF/ XSFILE(6,5,MXMOL),XSNAME(MXMOL),ALIAS(4,MXMOL)     FX00560
      COMMON /XSECTR/ V1FX(5,MXMOL),V2FX(5,MXMOL),DVFX(5,MXMOL),         FX00570
     *                WXM(MXMOL),NTEMPF(5,MXMOL),NSPECR(MXMOL),          FX00580
     *                IXFORM(5,MXMOL),XSMASS(MXMOL),XDOPLR(5,MXMOL),     FX00590
     *                NUMXS,IXSBIN                                       FX00595
C                                                                        FX00600
C     AMOLX(L,I)=MIXING RATIO (PPMV) OF THE I'TH MOLECULE FOR THE L'TH   FX00610
C     LEVEL, ALTX(L)= ALTITUDE OF THE L'TH LEVEL, LAYXMX LEVELS MAX      FX00620
C                                                                        FX00630
      COMMON /MLATMX/ LAYXMX,ALTX(MXZMD),AMOLX(MXZMD,MXMOL)              FX00640
      COMMON /PROFILE/ NLAYRS,PBAR(MXLAY),TBAR(MXLAY),
     *                 PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /MANE1/ P0,TEMP0,DVXM,H2OSLF,WTOT,ALBAR,ADBAR,AVBAR,
     *                 AVFIX,LAYRFX,SECNT0,SAMPLE,DVSET,ALFAL0,AVMASS,
     *                 DPTMIN,DPTFAC,ALTAV,AVTRAT,TDIFF1,TDIFF2,ALTD1,
     *                 ALTD2,ANGLE,IANT,LTGNT,LH1,LH2,IPFLAG,PLAY,TLAY,
     *                 EXTID(10)
      COMMON /SPECIES/ COLDRY(MXLAY),AMOUNT(MXMOL,MXLAY),WN2L(MXLAY),
     *                 CDUM(MXLAY),NMOLEC
      COMMON /PATHD1/ DVL(MXLAY),WTOTL(MXLAY),
     *                 ALBL(MXLAY),ADBL(MXLAY),AVBL(MXLAY),
     *                 H2OSL(MXLAY),IPATH(MXLAY),ITYL(MXLAY),
     *                 SECNTA(MXLAY),HT1,HT2,ALTZ(0:MXLAY)
C                                                                        FX00750
C     IXMAX=MAX NUMBER OF X-SECTION MOLECULES, IXMOLS=NUMBER OF THESE    FX00760
C     MOLECULES SELECTED, IXINDX=INDEX VALUES OF SELECTED MOLECULES      FX00770
C     (E.G. 1=CLONO2), XAMNT(I,L)=LAYER AMOUNTS FOR I'TH MOLECULE FOR    FX00780
C     L'TH LAYER, ANALOGOUS TO AMOUNT IN /PATHD/ FOR THE STANDARD        FX00790
C     MOLECULES.                                                         FX00800
C                                                                        FX00810
      COMMON /PATHX/ IXMAX,IXMOLS,IXINDX(MXMOL),XAMNT(MXMOL,MXLAY)       FX00820
      COMMON /ZOUTP/ ZOUT(MXLAY),SOUT(MXLAY),RHOSUM(MXLAY),              FX00830
     *               AMTTOT(MXMOL),AMTCUM(MXMOL),ISKIP(MXMOL)            FX00840
      COMMON /PCHINF/ MUNITS,CTYPE(MXLAY)                                       
C                                                                        FX00850
      DIMENSION XAMNTT(MXMOL)                                            FX00860
C                                                                        FX00870
      CHARACTER*48 CFORM1,CFORM2                                         FX00880
      CHARACTER*10 HOTHER                                                FX00890
      CHARACTER*7 PAFORM(2)                                              FX00900
      CHARACTER*4 PZFORM(5),ht1,ht2                                      FX00910
      CHARACTER*3 CTYPE                                                         
C                                                                        FX00920
      DATA HOTHER / ' OTHER    '/                                        FX00930
      DATA PZFORM / 'F8.6','F8.5','F8.4','F8.3','F8.2'/                  FX00940
      DATA PAFORM / '1PE15.7','  G15.7'/                                 FX00950
      DATA CFORM1 / '(1PE15.7,0PF10.2,10X,A3,I2,1X,2(F7.3,F8.3,F7.2))'/  FX00960
      DATA CFORM2 / '(  G15.7,0PF10.2,10X,A3,I2,23X,(F7.3,F8.3,F7.2))'/  FX00970
C                                                                        FX00980
      WRITE (IPR,900)                                                    FX00990
C                                                                        FX01000
      NOZSAV = NOZERO                                                    FX01010
      NOZERO = 1                                                         FX01020
      IOMXSV = IOUTMX                                                    FX01030
C                                                                        FX01040
C     READ IN THE NUMBER OF MOLECULES IXMOLS, AND THE FLAG IPRFL         FX01050
C     INDICATING WHETHER A STANDARD PROFILE (0) OR A USER-INPUT PROFILE  FX01060
C     (1) WILL BE USED.                                                  FX01070
C                                                                        FX01080
      READ (IRD,905) IXMOLS,IPRFL,IXSBIN                                 FX01090
      WRITE (IPR,910) IXMOLS,IPRFL                                       FX01100
C                                                                        FX01110
      IF (IPRFL.EQ.0) THEN                                               FX01120
         WRITE (IPR,915)                                                 FX01130
      ELSEIF (IPRFL.EQ.1) THEN                                           FX01140
         WRITE (IPR,920)                                                 FX01150
      ELSE                                                               FX01160
         WRITE (IPR,925)                                                 FX01170
         STOP 'STOPPED IN XAMNTS'                                        FX01180
      ENDIF                                                              FX01190
C                                                                        FX01200
C     READ IN DESIRED 'CROSS SECTIONS'                                   FX01210
C                                                                        FX01220
      CALL XSREAD (XV1,XV2)                                              FX01230
C                                                                        FX01240
      WRITE (IPR,930) (I,XSNAME(I),I=1,IXMOLS)                           FX01250
C                                                                        FX01260
C     CALL XPROFL TO GENERATE THE DENSITY PROFILES OF THE CROSS-SECTION  FX01270
C     MOLECULES.  THE PROFILES OF THE X-MOLECULES WILL BE STORED IN      FX01280
C     DENM(J,I), J=1,IXMOLS, I=1,IMMAX, AT THE LEVELS ZMDL(IMMAX)        FX01290
C                                                                        FX01300
      CALL XPROFL (IPRFL)                                                FX01310
C                                                                        FX01320
C     SET NMOL = IXMOLS FOR THE AMOUNT CALCULATION, BUT RESET IT AFTER.  FX01330
C     ALSO SET NOPRNT TO 1 TO SUPRESS PRINTING IN THE RAYTRACE.          FX01340
C                                                                        FX01350
      NMOLSV = NMOL                                                      FX01360
      NMOL = IXMOLS                                                      FX01370
      NOPRSV = NOPRNT                                                    FX01380
      NOPRNT = 1                                                         FX01390
C                                                                        FX01400
      DO 10 I = 1, IOUTDM                                                FX01410
         DO 8  K = 1, IXMAX                                              FX01420
            XAMNT(K,I) = 0.0                                             FX01430
 8       CONTINUE                                                               
 10   CONTINUE                                                           FX01440
C                                                                        FX01450
C     GET THE STANDARD-FORM SLANT PATH PARAMETERS H1, H2, ANGLE, PHI,    FX01460
C     HMIN, AND LEN FROM /ADRIVE/ AS H1F, H2F, ANGLEF, PHIF, HMINF       FX01470
C     AND LENF. USE THESE AS THE INPUTS TO THE RAYTRACE SUBROUTINE       FX01480
C     RFPATH TO CALCULATE THE ABSORBER AMOUNTS.                          FX01490
C                                                                        FX01500
      IF (ITYPE.EQ.1) THEN                                               FX01510
C                                                                        FX01520
C     =>  HORIZONTAL PATH                                                FX01530
C                                                                               
C        > GET NUMBER DENSITIES OF X-MOLECULES AT H1F <                  FX01540
C                                                                        FX01550
         IF (IMMAX.EQ.1) THEN                                            FX01560
C                                                                        FX01570
C           > ITYPE = 1, HOMOGENOUS PATH <                               FX01580
C                                                                        FX01590
            PH = PM(1)                                                   FX01600
            TH = TM(1)                                                   FX01610
            DO 20 K = 1, IXMOLS                                          FX01620
               DENP(K,1) = DENM(K,1)                                     FX01630
   20       CONTINUE                                                     FX01640
C                                                                        FX01650
         ELSE                                                            FX01660
C                                                                        FX01670
C           > INTERPOLATE NUMBER DENSITIES TO H1F <                      FX01680
C                                                                        FX01690
            ZH = H1F                                                     FX01700
            DO 30 L = 1, IMMAX                                           FX01710
               IF (ZH.LT.ZMDL(L)) GO TO 40                               FX01720
   30       CONTINUE                                                     FX01730
            L = IMMAX                                                    FX01740
   40       CONTINUE                                                     FX01750
            A = (ZH-ZMDL(L-1))/(ZMDL(L)-ZMDL(L-1))                       FX01760
            CALL EXPINT (PH,PM(L-1),PM(L),A)                             FX01770
            TH = TM(L-1)+(TM(L)-TM(L-1))*A                               FX01780
            DO 50 K = 1, IXMOLS                                          FX01790
               CALL EXPINT (DENP(K,1),DENM(K,L-1),DENM(K,L),A)           FX01800
   50       CONTINUE                                                     FX01810
         ENDIF                                                           FX01820
C                                                                        FX01830
C     > CALCULATE PATH AMOUNTS <                                         FX01840
C                                                                        FX01850
         DO 60 K = 1, IXMOLS                                             FX01860
            XAMNT(K,1) = DENP(K,1)*RANGEF*1.0E+5                         FX01870
   60    CONTINUE                                                        FX01880
         RANGE = RANGEF                                                  FX01890
C                                                                        FX01900
         LMAX = NLAYRS                                                   FX01910
         IOUTMX = LMAX+1                                                 FX01920
C                                                                        FX01930
      ELSE                                                               FX01940
C                                                                        FX01950
C     => SLANT PATH                                                      FX01960
C                                                                               
C        > ZERO OUT ARRAYS <                                             FX01970
C                                                                        FX01980
         DO 70 N = 1, IPDIM                                              FX01990
            IF (N.LE.IPDIM-2) THEN                                       FX02000
               ZPTH(N) = 0.0                                             FX02010
               PP(N) = 0.0                                               FX02020
               TP(N) = 0.0                                               FX02030
               RFNDXP(N) = 0.0                                           FX02040
               SP(N) = 0.0                                               FX02050
               PPSUM(N) = 0.0                                            FX02060
               TPSUM(N) = 0.0                                            FX02070
               RHOPSM(N) = 0.0                                           FX02080
            ENDIF                                                        FX02090
            DO 68 M = 1, KDIM                                            FX02100
               DENP(M,N) = 0.0                                           FX02110
               AMTP(M,N) = 0.0                                           FX02120
 68         CONTINUE                                                            
 70      CONTINUE                                                        FX02130
C                                                                        FX02140
C        > CALCULATE THE REFRACTIVITY <                                  FX02150
C                                                                        FX02160
         WRITE(IPR,*) '   - Using LOWTRAN6 refractive index -'                  
C                                                                               
         DO 80 IM = 1, IMMAX                                             FX02170
            PPH2O = AMOLS(IM,1)*PZERO*TM(IM)/(TZERO*ALOSMT)              FX02180
C                                                                               
C	    Approximation to refraction index (from LOWTRAN5)                         
C                                                                               
C           RFNDXM(IM) = ((77.46+0.459E-8*XVBAR**2)*PM(IM)/TM(IM)-              
C    *                   (PPH2O/1013.0)*(43.49-0.347E-8*XVBAR**2))*             
C    *                   1.0E-6                                                 
C                                                                               
C	    Approximation to refraction index (from LOWTRAN6)                         
C                                                                               
            RFNDXM(IM)=((83.42+(185.08/(1.0-(XVBAR/1.14E+5)**2))+        FX02190
     *    (4.11/(1.0-(XVBAR/6.24E+4)**2)))*(PM(IM)*288.15)/              FX02200
     *    (1013.25*TM(IM))-(43.49-(XVBAR/1.7E+4)**2)*(PPH2O/1013.25))           
     *    *1.0E-06                                                              
   80    CONTINUE                                                        FX02210
         CALL RFPATH (H1F,H2F,ANGLEF,PHIF,LENF,HMINF,1,RANGE,BETA,       FX02220
     *                BENDNG)                                            FX02230
C                                                                        FX02240
C        > CROSS-SECTION ABSORBER AMOUNTS ARE NOW IN AMTP(J,I).   <      FX02250
C        > CONDENSE THE AMOUNTS INTO THE LAYERS DEFINDED BY ZOUT. <      FX02260
C                                                                        FX02270
         I2 = IPMAX-1                                                    FX02280
         IOUT = 1                                                        FX02290
         DO 100 IP = 1, I2                                               FX02300
C                                                                        FX02310
            DO 90 K = 1, IXMOLS                                          FX02320
               XAMNT(K,IOUT) = XAMNT(K,IOUT)+AMTP(K,IP)                  FX02330
   90       CONTINUE                                                     FX02340
            IF (ZPTH(IP+1).EQ.ZOUT(IOUT+1)) IOUT = IOUT+1                FX02350
C                                                                        FX02360
  100    CONTINUE                                                        FX02370
C                                                                        FX02380
         IF (IOUT.NE.IOUTMX) THEN                                        FX02390
            WRITE (IPR,935) IOUT,IOUTMX                                  FX02400
            STOP 'STOPPED IN XAMNTS, IOUT .NE. IOUTMX'                   FX02410
         ENDIF                                                           FX02420
C                                                                        FX02430
         IOUTMX = IOMXSV                                                 FX02440
         LMAX = IOUTMX-1                                                 FX02450
C                                                                               
      ENDIF                                                              FX02460
C                                                                        FX02470
C     CROSS-SECTION AMOUNTS ARE NOW IN XAMNT. PRINT THEM OUT.            FX02480
C                      (in E15.7 format)                                        
C                                                                        FX02490
      IF (IPUNCH.EQ.1) THEN                                              FX02500
         IFRMX = 1                                                              
         WRITE (IPU,940) IXMOLS,IXSBIN                                   FX02510
         WRITE (IPU,945) (XSNAME(K),K=1,7),HOTHER,(XSNAME(K),K=8,NMOL)   FX02520
         IF (ITYPE.EQ.1) THEN                                            FX02530
            WRITE (IPU,950) IFRMX,LMAX,NMOL,SECNT0,HMOD,RANGE,ZH         FX02540
C                                                                               
C           -------------------------------------                               
C           > Write molecular information in    <                               
C           >  - mixing ratio if MUNITS is 1    <                               
C           >  - column density if MUNITS is 0  <                               
C           -------------------------------------                               
C                                                                               
            IF (MUNITS.EQ.1) THEN                                               
               DRAIR = WN2L(1)                                                  
               DO 105 M = 2,NMOLSV                                              
                  DRAIR = DRAIR + AMOUNT(M,1)                                   
 105           CONTINUE                                                         
C                                                                               
C              > If DRAIR is zero, then write out XAMNT only    <               
C              > (since XAMNT zero => mixing ratio zero)        <               
C                                                                               
               IF (DRAIR.EQ.0) THEN                                             
                  WRITE (IPU,955) PH,TH,IPATH(1),ZH,ZH,                         
     *                            (XAMNT(K,1),K=1,7),WN2L(1),                   
     *                            (XAMNT(K,1),K=8,NMOL)                         
               ELSE                                                             
                  WRITE (IPU,955) PH,TH,IPATH(1),ZH,ZH,                         
     *                            (XAMNT(K,1)/DRAIR,K=1,7),WN2L(1),             
     *                            (XAMNT(K,1)/DRAIR,K=8,NMOL)                   
               ENDIF                                                            
            ELSE                                                                
C                                                                               
C              Test to make sure there are no fractional molecular              
C              amounts written out (will cause PATH to assume                   
C              mixing ratio)                                                    
C                                                                               
               DO 107 K=1,NMOL                                                  
                  IF (XAMNT(K,1).LT.1.) THEN                                    
                     WRITE(IPR,1000) K,1                                        
                     XAMNT(K,1) = 0.0                                           
                  ENDIF                                                         
 107           CONTINUE                                                         
C                                                                               
               WRITE (IPU,955) PH,TH,IPATH(1),ZH,ZH,                     FX02550
     *                         (XAMNT(K,1),K=1,7),WN2L(1),                      
     *                         (XAMNT(K,1),K=8,NMOL)                     FX02560
            ENDIF                                                               
         ELSE                                                            FX02570
            WRITE (IPU,960) IFRMX,LMAX,NMOL,SECNT0,(HMOD(I),I=1,2),      FX02580
     *                      H1F,H2F,ANGLE,LENF                           FX02590
         ENDIF                                                           FX02600
      ENDIF                                                              FX02610
C                                                                        FX02620
      WRITE (IPR,965) (XSNAME(I),I=1,IXMOLS)                             FX02630
C                                                                        FX02640
      DO 110 K = 1, IXMOLS                                               FX02650
         XAMNTT(K) = 0.0                                                 FX02660
  110 CONTINUE                                                           FX02670
C                                                                        FX02680
      DO 130 L = 1, NLAYRS                                               FX02690
C                                                                               
C        > Write atmosphere to TAPE6 in column density <                        
C                                                                               
         IF (ITYPE.EQ.1) THEN                                            FX02700
            WRITE (IPR,970) L,ZOUT(L),ZOUT(L),(XAMNT(K,L),K=1,IXMOLS)    FX02710
         ELSE                                                            FX02720
            WRITE (IPR,970) L,ZOUT(L),ZOUT(L+1),(XAMNT(K,L),K=1,IXMOLS)  FX02730
         ENDIF                                                           FX02740
         DO 120 K = 1, IXMOLS                                            FX02750
            FAC = 1.0                                                    FX02760
            IF (IPATH(L).EQ.2) FAC = 2.0                                 FX02770
            XAMNTT(K) = XAMNTT(K)+FAC*XAMNT(K,L)                         FX02780
  120    CONTINUE                                                        FX02790
C                                                                        FX02800
         IF (IPUNCH.EQ.1.AND.ITYPE.NE.1) THEN                            FX02810
            LTST = L                                                     FX02820
            IF (L.EQ.1) LTST = 0                                         FX02830
            PTST =  LOG10(PZ(LTST))                                      FX02840
            NPTST = PTST+2                                               FX02850
            IF (PTST.LT.0.0) NPTST = 1                                   FX02860
            CFORM1(38:41) = PZFORM(NPTST)                                FX02870
            CFORM2(38:41) = PZFORM(NPTST)                                FX02880
            NPTST = 1                                                    FX02890
            IF (PBAR(L).GE.0.1) NPTST = 2                                FX02900
            CFORM1(2:8) = PAFORM(NPTST)                                  FX02910
            CFORM2(2:8) = PAFORM(NPTST)                                  FX02920
            IF (L.EQ.1) THEN                                             FX02930
               WRITE (IPU,CFORM1) PBAR(L),TBAR(L),CTYPE(L),IPATH(L),            
     *                            ALTZ(L-1),PZ(L-1),TZ(L-1),ALTZ(L),     FX02940
     *                            PZ(L),TZ(L)                            FX02950
            ELSE                                                         FX02960
               WRITE (IPU,CFORM2) PBAR(L),TBAR(L),CTYPE(L),IPATH(L),     FX02970
     *                            ALTZ(L),PZ(L),TZ(L)                    FX02980
            ENDIF                                                        FX02990
C                                                                               
C           -------------------------------------                               
C           > Write molecular information in    <                               
C           >  - mixing ratio if MUNITS is 1    <                               
C           >  - column density if MUNITS is 0  <                               
C           -------------------------------------                               
C                                                                               
            IF (MUNITS.EQ.1) THEN                                               
               DRAIR = WN2L(L)                                                  
               DO 125 M = 2,NMOLSV                                              
                  DRAIR = DRAIR + AMOUNT(M,L)                                   
 125           CONTINUE                                                         
C                                                                               
C              > If DRAIR is zero, then write out XAMNT only    <               
C              > (since XAMNT zero => mixing ratio zero)        <               
C                                                                               
               IF (DRAIR.EQ.0) THEN                                             
                  WRITE (IPU,975) (XAMNT(K,L),K=1,7),WN2L(L)                    
                  IF (NMOL.GT.7) WRITE (IPU,975) (XAMNT(K,L),K=8,NMOL)          
               ELSE                                                             
                  WRITE (IPU,975) (XAMNT(K,L)/DRAIR,K=1,7),WN2L(L)              
                  IF (NMOL.GT.7) WRITE (IPU,975)                                
     *                                 (XAMNT(K,L)/DRAIR,K=8,NMOL)              
               ENDIF                                                            
            ELSE                                                                
C                                                                               
C              Test to make sure there are no fractional molecular              
C              amounts written out (will cause PATH to assume                   
C              mixing ratio)                                                    
C                                                                               
               DO 127 K=1,NMOL                                                  
                  IF (XAMNT(K,L).LT.1.) THEN                                    
                     WRITE(IPR,1000) K,L                                        
                     XAMNT(K,L) = 0.0                                           
                  ENDIF                                                         
 127           CONTINUE                                                         
C                                                                               
               WRITE (IPU,975) (XAMNT(K,L),K=1,7),WN2L(L)                FX03000
               IF (NMOL.GT.7) WRITE (IPU,975) (XAMNT(K,L),K=8,NMOL)      FX03010
            ENDIF                                                               
         ENDIF                                                           FX03020
C                                                                        FX03030
  130 CONTINUE                                                           FX03040
C                                                                        FX03050
C     > Write atmosphere to TAPE6 in mixing ratio <                             
C                                                                               
      WRITE(IPR,973)                                                            
      DO 135 L = 1, NLAYRS                                                      
         DRAIR = WN2L(L)                                                        
         DO 133 M = 2,NMOLSV                                                    
            DRAIR = DRAIR + AMOUNT(M,L)                                         
 133     CONTINUE                                                               
C                                                                               
C        > If DRAIR is zero, then write out XAMNT only    <                     
C        > (since XAMNT zero => mixing ratio zero)        <                     
C                                                                               
         IF (DRAIR.EQ.0) THEN                                                   
            IF (ITYPE.EQ.1) THEN                                                
               WRITE (IPR,970) L,ZOUT(L),ZOUT(L),                               
     *                         (XAMNT(K,L),K=1,IXMOLS)                          
            ELSE                                                                
               WRITE (IPR,970) L,ZOUT(L),ZOUT(L+1),                             
     *                         (XAMNT(K,L),K=1,IXMOLS)                          
            ENDIF                                                               
         ELSE                                                                   
            IF (ITYPE.EQ.1) THEN                                                
               WRITE (IPR,970) L,ZOUT(L),ZOUT(L),                               
     *                         (XAMNT(K,L)/DRAIR,K=1,IXMOLS)                    
            ELSE                                                                
               WRITE (IPR,970) L,ZOUT(L),ZOUT(L+1),                             
     *                         (XAMNT(K,L)/DRAIR,K=1,IXMOLS)                    
            ENDIF                                                               
         ENDIF                                                                  
 135  CONTINUE                                                                  
C                                                                               
      WRITE (IPR,980) (XAMNTT(K),K=1,IXMOLS)                             FX03060
C                                                                        FX03070
C     DONE                                                               FX03080
C                                                                        FX03090
      NOZERO = NOZSAV                                                    FX03100
      NMOL = NMOLSV                                                      FX03110
      NOPRNT = NOPRSV                                                    FX03120
C                                                                        FX03130
      RETURN                                                             FX03140
C                                                                        FX03150
  900 FORMAT ('1***** XAMNTS: ABSORBER AMOUNTS FOR THE CROSS-',          FX03160
     *        'SECTION MOLECULES *****')                                 FX03170
  905 FORMAT (3I5)                                                       FX03180
  910 FORMAT (/,'     IXMOLS      ISTD',/,2I10)                          FX03190
  915 FORMAT (/,'     USER INPUT PROFILE SELECTED')                      FX03200
  920 FORMAT (/,'     STANDARD PROFILE SELECTED')                        FX03210
  925 FORMAT (/,'  ERROR: IPRFL IS NOT 0 OR 1, STOP')                    FX03220
  930 FORMAT (/,'  THE CROSS-SECTION MOLECULES SELECTED ARE: ',/,/,      FX03230
     *        (5X,I5,3X,A))                                              FX03240
  935 FORMAT (/,'  XAMNTS: ERROR- IOUT = ',I5,                           FX03250
     *        '  DOES NOT MATCH IOUTMX = ',I5)                           FX03260
  940 FORMAT (2(I5,5X),' THE FOLLOWING CROSS-SECTIONS WERE SELECTED:')   FX03270
  945 FORMAT (8A10)                                                      FX03280
  950 FORMAT (1X,I1,I3,I5,F10.6,3A8,' * ',F7.3,' KM PATH AT ',F7.3,      FX03290
     *        ' KM ALT')                                                 FX03300
  955 FORMAT (E15.7,F10.4,10X,I5,1X,F7.3,15X,F7.3,/,(1P8E15.7))          FX03310
  960 FORMAT (1X,I1,I3,I5,F10.6,2A8,' H1=',F8.3,' H2=',F8.3,' ANG=',     FX03320
     *        F8.3,' LEN=',I2)                                           FX03330
  965 FORMAT (//,'  LAYER AMOUNTS FOR THE CROSS-SECTION MOLECULES',//,   FX03340
     *        '           LAYER          AMOUNTS (MOLS/CM2)',/,          FX03350
     *        '   L    FROM     TO ',/,'        (KM)    (KM)',4X,8A10,   FX03360
     *        /,25X,8A10)                                                FX03370
  970 FORMAT (1X,I3,2F8.3,3X,1P8E10.3,/,23X,1P8E15.7)                    FX03380
  973 FORMAT ('1',3X,'------------------------------------',/,                  
     *            3X,'  MOLECULAR MIXING RATIOS BY LAYER',/)                    
  975 FORMAT (1P8E15.7)                                                  FX03390
  980 FORMAT (//,1X,'TOTAL AMOUNT FOR PATH ',1P8E15.7)                   FX03400
 1000 FORMAT ('*** WARNING: Zeroing molecule #',i2.2,' amount ',                
     *        'in layer #',i3.3)                                                
C                                                                        FX03410
      END                                                                FX03420
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE XPROFL (IPRFL)                                          FX03430
C                                                                        FX03440
C     *****************************************************************  FX03450
C     THIS SUBROUTINE GENERATES THE DENSITY PROFILES OF THE CROSS-       FX03460
C     SECTION MOLECULES.  IT STORES THE PROFILES IN THE ARRAY DENM IN    FX03470
C     /DEAMT/ AT THE ALTITUDES ZMDL, WHICH ARE THE SAME ALTITUDES THAT   FX03480
C     THE PROFILES OF THE MOLECULAR AMOUNTS ARE DEFINED ON.  (NOTE: THE  FX03490
C     ACTUAL ALTITUDES USED ARE FROM ZST WHICH IS A COPY OF ZMDL.)       FX03500
C     IPRFL IS A FLAG INDICATING THAT THE STANDARD PROFILES (0) OR A     FX03510
C     USER-INPUT PROFILE (1) IS TO BE USED.                              FX03520
C     *****************************************************************  FX03530
C                                                                        FX03540
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FX03550
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FX03560
C                                                                        FX03570
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FX03580
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FX03590
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FX03600
C                                                                        FX03610
      CHARACTER*8      HMOD                                             &FX03620
C                                                                        FX03630
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
                                                                        FX03680 
C     IFIL CARRIES FILE INFORMATION                                      FX03690
C                                                                        FX03700
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FX03710
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FX03720
     *              NLTEFL,LNFIL4,LNGTH4                                 FX03730
C                                                                        FX03740
C     LAMCHN CARRIES HARDWARE SPECIFIC PARAMETERS                        FX03750
C                                                                        FX03760
      COMMON /LAMCHN/ ONEPL,ONEMI,EXPMIN,ARGMIN                          FX03770
                                                                                
      COMMON /c_drive/ ref_lat,hobs,co2mx,ibmax_b,immax_b,                      
     *                 lvl_1_2,jchar_st(10,2),wm(mxzmd)                         
c                                                                               
      character*1 jchar_st                                                      
c                                                                               
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FX03790
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FX03800
C                                                                        FX03810
C     IXMAX=MAX NUMBER OF X-SECTION MOLECULES, IXMOLS=NUMBER OF THESE    FX03820
C     MOLECULES SELECTED, IXINDX=INDEX VALUES OF SELECTED MOLECULES      FX03830
C     (E.G. 1=CLONO2), XAMNT(I,L)=LAYER AMOUNTS FOR I'TH MOLECULE FOR    FX03840
C     L'TH LAYER, ANALOGOUS TO AMOUNT IN /PATHD/ FOR THE STANDARD        FX03850
C     MOLECULES.                                                         FX03860
C                                                                        FX03870
      COMMON /PATHX/ IXMAX,IXMOLS,IXINDX(MXMOL),XAMNT(MXMOL,MXLAY)       FX03880
      COMMON /MLATM/ ALT(MXZMD),PMDL(MXZMD,6),TMDL(MXZMD,6),             FX03890
     *               AMOL(MXZMD,8,6),ZST(MXZMD),PST(MXZMD),              FX03900
     *               TST(MXZMD),AMOLS(MXZMD,MXMOL)                       FX03910
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FX03920
C                                                                        FX03930
C     COMMON BLOCKS AND PARAMETERS FOR THE PROFILES AND DENSITIES        FX03940
C     FOR THE CROSS-SECTION MOLECULES.                                   FX03950
C     XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES         FX03960
C                                                                        FX03970
      CHARACTER*10 XSFILE,XSNAME,ALIAS                                   FX03980
      COMMON /XSECTF/ XSFILE(6,5,MXMOL),XSNAME(MXMOL),ALIAS(4,MXMOL)     FX03990
C                                                                        FX04000
C     AMOLX(L,I)=MIXING RATIO (PPMV) OF THE I'TH MOLECULE FOR THE L'TH   FX04010
C     LEVEL, ALTX(L)= ALTITUDE OF THE L'TH LEVEL, LAYXMX LEVELS MAX      FX04020
C                                                                        FX04030
      COMMON /MLATMX/ LAYXMX,ALTX(MXZMD),AMOLX(MXZMD,MXMOL)              FX04040
                                                                         FX04050
      DIMENSION ZX(MXZMD),DTMP(MXMOL,MXZMD),DENX(MXMOL,MXZMD)            FX04060
      DIMENSION PX(MXZMD)                                                       
      DIMENSION ZTMP(2),PTMP(2),TTMP(2),WVTMP(2)                                
      CHARACTER JCHAR(MXMOL,MXZMD)*1,XTITLE*50                           FX04070
C                                                                        FX04080
C     LOAD THE PROFILES OF ALTITUDE, PRESSURE, AND TEMPERATURE THAT      FX04090
C     WERE USED TO CALCULATE THE MOLECULAR AMOUNTS BACK INTO THE         FX04100
C     ARRAYS ZMDL, PM, AND TM FROM THE ARRAYS ZST, PST, AND TST          FX04110
C                                                                        FX04120
      DO 10 L = 1, IMMAX                                                 FX04130
         ZMDL(L) = ZST(L)                                                FX04140
         PM(L) = PST(L)                                                  FX04150
         TM(L) = TST(L)                                                  FX04160
   10 CONTINUE                                                           FX04170
C                                                                        FX04180
      IF (IPRFL.GT.0) THEN                                               FX04190
C                                                                        FX04200
C     A STANDARD PROFILE FOR X-MOLECULES DENSITY PROFILES HAS BEEN       FX04210
C     SELECTED. THE PROFILES OF VOLUME MIXING RATIO ARE IN AMOLX         FX04220
C     STORED AT THE LEVELS ALTX. LOAD THE ALTITUDES INTO ZX AND          FX04230
C     DENX RESPECTIVELY.                                                 FX04240
C                                                                        FX04250
         LAYX = LAYXMX                                                   FX04260
         DO 30 L = 1, LAYX                                               FX04270
            ZX(L) = ALTX(L)                                              FX04280
            DO 20 K = 1, IXMOLS                                          FX04290
               DENX(K,L) = AMOLX(L,IXINDX(K))                            FX04300
   20       CONTINUE                                                     FX04310
   30    CONTINUE                                                        FX04320
C                                                                        FX04330
      ELSE                                                               FX04340
C                                                                        FX04350
C     A USER-INPUT PROFILE HAS BEEN SELECTED. READ IN THE PROFILES       FX04360
C     AND INTERPOLATE THEM TO THE LEVELS ZMDL.                           FX04370
C                                                                        FX04380
C     READ IN THE PROFILES. NOTE THAT ZORP CAN BE EITHER ALTITUDE        FX04390
C     OR PRESSURE, DEPENDING UPON THE VALUE OF IZORP: 0 FOR              FX04400
C     ALTITUDE, 1 FOR PRESSURE.                                          FX04410
C                                                                        FX04420
         WRITE (IPR,900)                                                 FX04430
         READ (IRD,905) LAYX,IZORP,XTITLE                                FX04440
         WRITE (IPR,910) LAYX,IZORP,XTITLE                               FX04450
         IF (LAYX.GT.LAYXMX) THEN                                        FX04460
            WRITE (IPR,915) LAYXMX                                       FX04470
            STOP 'STOPPED IN XPROFL'                                     FX04480
         ENDIF                                                           FX04490
C                                                                        FX04500
         WRITE (IPR,920) (K,K=1,IXMOLS)                                  FX04510
C                                                                        FX04520
         IF (IZORP .EQ. 0) THEN                                                 
            DO 50 L = 1, LAYX                                               FX04
               READ (IRD,925) ZX(L),(JCHAR(I,L),I=1,IXMOLS)                 FX04
               WRITE (IPR,930) ZX(L),(JCHAR(I,L),I=1,IXMOLS)                FX04
C                                                                           FX04
               READ (IRD,935) (DTMP(K,L),K=1,IXMOLS)                        FX04
               WRITE (IPR,940) (DTMP(K,L),K=1,IXMOLS)                       FX04
 50         CONTINUE                                                        FX04
         ELSE                                                                   
            DO 60 L = 1, LAYX                                               FX04
               READ (IRD,925) PX(L),(JCHAR(I,L),I=1,IXMOLS)                 FX04
               WRITE (IPR,930) PX(L),(JCHAR(I,L),I=1,IXMOLS)                FX04
C                                                                           FX04
               READ (IRD,935) (DTMP(K,L),K=1,IXMOLS)                        FX04
               WRITE (IPR,940) (DTMP(K,L),K=1,IXMOLS)                       FX04
 60         CONTINUE                                                            
         ENDIF                                                                  
c                                                                               
         IF (IZORP .EQ. 1) THEN                                                 
c                                                                               
C INTERPOLATE PX GRID ONTO ZX GRID.                                             
                                                                                
C TO ENSURE THAT CALCULATED/INPUT ZMDL'S WILL MATCH CALCULATED USER-LEVEL       
C ALTITUDES, A COMBINATION OF INTERPOLATION AND HYDROSTATICS ARE USED.          
C ZBND = A * F1(P) + (1 - A) * F2(P), WHERE                                     
C F1(P) = INTERPOLATION IN LN(P), F2(P) = HYDROSTATIC CALCULATION               
            ISTART = 2                                                          
                                                                                
            DO 160 IP=1,LAYX                                                    
               PTMP(1) = 0.0                                                    
               TTMP(1) = 0.0                                                    
               WVTMP(1) = 0.0                                                   
               ZTMP(1) = 0.0                                                    
                                                                                
               PTMP(2) = 0.0                                                    
               TTMP(2) = 0.0                                                    
               WVTMP(2) = 0.0                                                   
               ZTMP(2) = 0.0                                                    
                                                                                
               DO 161 LIP=ISTART,IMMAX                                          
                  IF (PX(IP) .GT. PM(LIP)) GO TO 162                            
 161           CONTINUE                                                         
               LIP=IMMAX                                                        
 162           CONTINUE                                                         
                                                                                
               IF (PX(IP) .EQ. PM(LIP-1)) THEN                                  
                  ZX(IP) = ZMDL(LIP-1)                                          
               ELSE                                                             
                                                                                
                  IF(PX(IP) .EQ. PM(LIP)) THEN                                  
                     ZX(IP) = ZMDL(LIP)                                         
                  ELSE                                                          
                                                                                
C     PERFORM INTERPOLATION IN LN(PM)                                           
                     HIP =  (ZMDL(LIP)-ZMDL(LIP-1))/                            
     &                    LOG(PM(LIP)/PM(LIP-1))                                
                     ZINT = ZMDL(LIP-1)+                                        
     &                    HIP* LOG(PX(IP)/PM(LIP-1))                            
                                                                                
C     PERFORM ALTITUDE CALCULATION USING HYDROSTATIC EQUATION                   
                     PTMP(1) = PM(LIP-1)                                        
                     ZTMP(1) = ZMDL(LIP-1)                                      
                     TTMP(1) = TM(LIP-1)                                        
                     WVTMP(1) = DENW(LIP-1)                                     
                                                                                
                     PTMP(2) = PX(IP)                                           
                                                                                
                     TIP = (TM(LIP)-TM(LIP-1))/                                 
     &                    LOG(PM(LIP)/PM(LIP-1))                                
                     TTMP(2) = TM(LIP-1)+                                       
     &                    TIP* LOG(PX(IP)/PM(LIP-1))                            
                                                                                
                     WVIP =  (DENW(LIP)-DENW(LIP-1))/                           
     &                    LOG(PM(LIP)/PM(LIP-1))                                
                     WVTMP(2) =  DENW(LIP-1) +                                  
     &                    WVIP* LOG(PX(IP)/PM(LIP-1))                           
                                                                                
                     CALL CMPALT(2,PTMP,TTMP,                                   
     &                    WVTMP,ZTMP(1),REF_LAT,ZTMP)                           
C     COMBINE THE INTERPOLATION AND THE HYDROSTATIC CALCULATION                 
                                                                                
                     RATP =  LOG(PX(IP)/PM(LIP-1))/                             
     &                    LOG(PM(LIP)/PM(LIP-1))                                
                                                                                
                     A = RATP**3                                                
                                                                                
                     ZX(IP) = A*ZINT + (1-A)*ZTMP(2)                            
                                                                                
                  ENDIF                                                         
               ENDIF                                                            
                                                                                
               IF (IP .NE. 1) THEN                                              
                  IF (ZX(IP).LE.ZX(IP-1)) GO TO 300                             
               ENDIF                                                            
                                                                                
               ISTART = LIP                                                     
               CALL XTRACT (IP,DTMP,JCHAR,ZX(IP))                               
               DO 40 K = 1, IXMOLS                                              
                  DENX(K,IP) = DTMP(K,IP)                                       
 40            CONTINUE                                                         
                                                                                
 160        CONTINUE                                                            
c                                                                               
         ELSE                                                                   
c            stop 'do 171'                                                      
c                                                                               
c	   izorp is zero to get to this                                               
c                                                                               
            DO 171 L=1,LAYX                                                     
               CALL XTRACT (L,DTMP,JCHAR,ZX(L))                                 
               DO 41 K = 1, IXMOLS                                              
                  DENX(K,L) = DTMP(K,L)                                         
 41            CONTINUE                                                         
 171        CONTINUE                                                            
         ENDIF                                                                  
c                                                                               
      ENDIF                                                                     
C                                                                        FX04810
C     INTERPOLATE THE DENSITY PROFILE DENX DEFINED ON ZX TO DENM         FX04820
C     DEFINED ON ZMDL, THEN CONVERT MIXING RATIO TO NUMBER DENSITY.      FX04830
C                                                                        FX04840
      CALL XINTRP (ZX,DENX,LAYX,IXMOLS)                                  FX04850
C                                                                        FX04860
      RETURN                                                             FX04870
                                                                                
C ERROR MESSAGES                                                                
  300 WRITE(IPR,988) (ZX(I),I=1,LAYX)                                           
      PRINT 988,(ZX(I),I=1,IP)                                                  
                                                                                
      STOP 'ZX IN XPROFL'                                                       
C                                                                        FX04880
  900 FORMAT (/,' READING IN A PROFILE FOR THE CROSS-SECTION',           FX04890
     *        ' MOLECULES')                                              FX04900
  905 FORMAT (2I5,A)                                                     FX04910
  910 FORMAT (/,'  LAYERS = ',I5,/,'     IZORP = ',I5,                   FX04920
     *        '  (0 FOR ALTITUDE, 1 FOR PRESSURE)',/,                    FX04930
     *        '  TITLE  = ',A50)                                         FX04940
  915 FORMAT (/,'  XPROFL: ERROR- LAYX > LAYXMX = ',I4)                  FX04950
  920 FORMAT (/,'      Z OR P     JCHAR',/,'  ',                         FX04960
     *        8('    DENX(',I2,')'))                                     FX04970
  925 FORMAT (F10.3,5X,38A1)                                             FX04980
  930 FORMAT (2X,F10.3,5X,38A1)                                           FX0499
  935 FORMAT (8E10.3)                                                    FX05000
  940 FORMAT (2X,1p,8E12.3)                                                 FX05
  988 FORMAT (///,' ERROR: BOUNDARY ALTITUDES FOR CROSS_SECTION LEVELS',        
     *        'ARE NEGATIVE OR NOT IN ASCENDING ORDER',//,5X,' ZX ',            
     *        /,(10F10.4))                                                      
C                                                                        FX05020
      END                                                                FX05030
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE XTRACT (ILEV,DTMP,JCHAR,Z)                              FX05040
C                                                                        FX05050
C     *****************************************************************  FX05060
C     FOR EACH MOLECULE K FOR WHICH JCHAR(K,ILEV) IS '1', THIS SUBROUTINEFX05070
C     INTERPOLATES THE MIXING RATIO DTMP(K,ILEV) AT THE ALTITUDE Z              
C     FROM THE STANDARD PROFILE IN AMOLX ON THE ALTITUDE GRID ALTX.      FX05090
C     *****************************************************************  FX05100
C                                                                        FX05110
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FX05120
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FX05130
C                                                                        FX05140
C     COMMON BLOCKS AND PARAMETERS FOR THE PROFILES AND DENSITIES        FX05150
C     FOR THE CROSS-SECTION MOLECULES.                                   FX05160
C     XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES         FX05170
C                                                                        FX05180
      CHARACTER*10 XSFILE,XSNAME,ALIAS                                   FX05190
      COMMON /XSECTF/ XSFILE(6,5,MXMOL),XSNAME(MXMOL),ALIAS(4,MXMOL)     FX05200
C                                                                        FX05210
C     AMOLX(L,I)=MIXING RATIO (PPMV) OF THE I'TH MOLECULE FOR THE L'TH   FX05220
C     LEVEL, ALTX(L)= ALTITUDE OF THE L'TH LEVEL, LAYXMX LEVELS MAX      FX05230
C                                                                        FX05240
      COMMON /MLATMX/ LAYXMX,ALTX(MXZMD),AMOLX(MXZMD,MXMOL)              FX05250
C                                                                        FX05260
C     IXMAX=MAX NUMBER OF X-SECTION MOLECULES, IXMOLS=NUMBER OF THESE    FX05270
C     MOLECULES SELECTED, IXINDX=INDEX VALUES OF SELECTED MOLECULES      FX05280
C     (E.G. 1=CLONO2), XAMNT(I,L)=LAYER AMOUNTS FOR I'TH MOLECULE FOR    FX05290
C     L'TH LAYER, ANALOGOUS TO AMOUNT IN /PATHD/ FOR THE STANDARD        FX05300
C     MOLECULES.                                                         FX05310
C                                                                        FX05320
      COMMON /PATHX/ IXMAX,IXMOLS,IXINDX(MXMOL),XAMNT(MXMOL,MXLAY)       FX05330
C                                                                        FX05340
      DIMENSION DTMP(MXMOL,MXZMD)                                        FX05350
      CHARACTER*1 JCHAR(MXMOL,MXZMD)                                     FX05360
C                                                                        FX05370
C     FIND SMALLEST ALTX(L) GT Z                                         FX05380
C                                                                        FX05390
      DO 10 L = 2, LAYXMX                                                FX05400
         IF (Z.LE.ALTX(L)) GO TO 20                                      FX05410
   10 CONTINUE                                                           FX05420
      L = LAYXMX                                                         FX05430
C                                                                        FX05440
   20 CONTINUE                                                           FX05450
C                                                                        FX05460
      DO 30 K = 1, IXMOLS                                                FX05470
         IF (JCHAR(K,ILEV).EQ.'1') THEN                                  FX05480
C                                                                        FX05490
C     INTERPOLATE MIXING RATIO FROM STANDARD PROFILE                     FX05500
C                                                                        FX05510
            A = (Z-ALTX(L-1))/(ALTX(L)-ALTX(L-1))                        FX05520
            CALL EXPINT (DTMP(K,ILEV),AMOLX(L,IXINDX(K)),                FX05530
     *                           AMOLX(L-1,IXINDX(K)),A)                 FX05540
         ENDIF                                                           FX05550
   30 CONTINUE                                                           FX05560
C                                                                        FX05570
      RETURN                                                             FX05580
C                                                                        FX05590
      END                                                                FX05600
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE XINTRP (ZX,DENX,LAYX,IXMOLS)                            FX05610
C                                                                        FX05620
C     *****************************************************************  FX05630
C     THIS SUBROUTINE INTERPLOLATES THE PROFILE DENX ON THE ALTITUDE     FX05640
C     GRID ZX INTO DENM ON THE GRID ZMDL.  EXPONENTIAL INTERPOLATION     FX05650
C     IS USED.                                                           FX05660
C     *****************************************************************  FX05670
C                                                                        FX05680
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FX05690
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FX05700
C                                                                        FX05710
C     IFIL CARRIES FILE INFORMATION                                      FX05720
C                                                                        FX05730
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FX05740
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FX05750
     *              NLTEFL,LNFIL4,LNGTH4                                 FX05760
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
      COMMON /CNSTATM/ PZERO,TZERO,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(MXMOL)        
C                                                                        FX05790
C     LAMCHN CARRIES HARDWARE SPECIFIC PARAMETERS                        FX05800
C                                                                        FX05810
      COMMON /LAMCHN/ ONEPL,ONEMI,EXPMIN,ARGMIN                          FX05820
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)      FX05830
      COMMON WPATH(IM2,16),TBBY(IM2)                                     FX05840
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)         FX05850
C                                                                        FX05860
      CHARACTER*8      HMOD                                             &FX05870
C                                                                        FX05880
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
      COMMON /MLATM/ ALT(MXZMD),PMDL(MXZMD,6),TMDL(MXZMD,6),             FX05930
     *               AMOL(MXZMD,8,6),ZST(MXZMD),PST(MXZMD),              FX05940
     *               TST(MXZMD),AMOLS(MXZMD,MXMOL)                       FX05950
      COMMON /DEAMT/ DENM(MXMOL,MXZMD),DENP(MXMOL,MXPDIM),DRYAIR(MXZMD)  FX05960
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FX05980
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                      FX05990
C                                                                        FX06000
      DIMENSION ZX(MXZMD),DENX(MXMOL,MXZMD)                              FX06010
C                                                                        FX06020
      LX = 2                                                             FX06030
      DO 30 L = 1, IMMAX                                                 FX06040
C                                                                        FX06050
C        > FIND THE SMALLEST ZX GE ZMDL(L) <                             FX06060
C                                                                        FX06070
   10    CONTINUE                                                        FX06080
         IF (ZMDL(L).LE.ZX(LX).OR.LX.EQ.LAYX) THEN                       FX06090
            A = (ZMDL(L)-ZX(LX-1))/(ZX(LX)-ZX(LX-1))                     FX06100
            IF (A.LT.0.0 .OR. A.GT.1.0) WRITE (IPR,900)                  FX06110
C                                                                        FX06120
C           > IF DRYAIR FOR LAYER NOT CALCULATED PREVIOUSLY <                   
C           > (USING NORMAL MOLECULES), THEN CALCULATE THE  <                   
C           > NUMBER DENSITY OF AIR                         <            FX06130
C                                                                        FX06140
            IF (DRYAIR(L).EQ.0.)                                                
     *           DRYAIR(L) = ALOSMT*(PM(L)/PZERO)/(TM(L)/TZERO)          FX06150
C                                                                        FX06160
            DO 20 K = 1, IXMOLS                                          FX06170
               CALL EXPINT (DENM(K,L),DENX(K,LX-1),DENX(K,LX),A)         FX06180
C                                                                        FX06190
C              > CONVERT MIXING RATIO (PPMV) TO NUMBER DENSITY <         FX06200
C                                                                        FX06210
               DENM(K,L) = DRYAIR(L)*DENM(K,L)*1.0E-6                    FX06220
   20       CONTINUE                                                     FX06230
            GO TO 30                                                     FX06240
         ELSE                                                            FX06250
            LX = LX+1                                                    FX06260
         ENDIF                                                           FX06270
         GO TO 10                                                        FX06280
C                                                                        FX06290
   30 CONTINUE                                                           FX06300
C                                                                        FX06310
      RETURN                                                             FX06320
C                                                                        FX06330
  900 FORMAT (//,'  XINTPL: CAUTION- EXTRAPOLATING X-SECTION PROFILE')   FX06340
C                                                                        FX06350
      END                                                                FX06360
C                                                                               
C -------------------------------------------------------------------           
C                                                                               
      BLOCK DATA XMLATM                                                  FX06590
C                                                                        FX06600
C     *****************************************************************  FX06610
C     THIS BLOCK DATA SUBROUTINE INITIALIZES THE STANDARD PROFILES       FX06620
C     FOR THE "CROSS-SECTION" MOLECULES, THAT IS, THE MOLECULES FOR      FX06630
C     WHICH THE SPECTRAL DATA IS IN THE FORM OF CROSS-SECTIONS           FX06640
C     (ABSORPTION COEFFICIENTS) INSTEAD OF LINE PARAMETERS.              FX06650
C     THE PROFILES OF VOLUME MIXING RATIOS GIVEN HERE ARE FROM:          FX06660
C                                                                        FX06670
C                ?????????????????????????????????                       FX06680
C                                                                        FX06690
C     *****************************************************************  FX06700
C                                                                        FX06710
C     COMMON BLOCKS AND PARAMETERS FOR THE PROFILES AND DENSITIES        FX06720
C     FOR THE CROSS-SECTION MOLECULES.                                   FX06730
C                                                                        FX06740
C     AMOLX(L,I)=MIXING RATIO (PPMV) OF THE I'TH MOLECULE FOR THE L'TH   FX06750
C     LEVEL, ALTX(L)= ALTITUDE OF THE L'TH LEVEL, LAYXMX LEVELS MAX      FX06760
C                                                                        FX06770
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                    FX06780
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)     FX06790
      PARAMETER (MXZ50=MXZMD-50)                                         FX06800
C                                                                        FX06810
      COMMON /MLATMX/ LAYXMX,ALTX(MXZMD),                                FX06820
     *                AMOL1(MXZMD),  AMOL2(MXZMD),  AMOL3(MXZMD),        FX06830
     *                AMOL4(MXZMD),  AMOL5(MXZMD),  AMOL6(MXZMD),        FX06840
     *                AMOL7(MXZMD),  AMOL8(MXZMD),  AMOL9(MXZMD),        FX06850
     *                AMOL10(MXZMD), AMOL11(MXZMD), AMOL12(MXZMD),       FX06860
     *                AMOL13(MXZMD), AMOL14(MXZMD), AMOL15(MXZMD),       FX06870
     *                AMOL16(MXZMD), AMOL17(MXZMD), AMOL18(MXZMD),       FX06880
     *                AMOL19(MXZMD), AMOL20(MXZMD), AMOL21(MXZMD),       FX06890
     *                AMOL22(MXZMD), AMOL23(MXZMD), AMOL24(MXZMD),       FX06900
     *                AMOL25(MXZMD), AMOL26(MXZMD), AMOL27(MXZMD),       FX06910
     *                AMOL28(MXZMD), AMOL29(MXZMD), AMOL30(MXZMD),       FX06920
     *                AMOL31(MXZMD), AMOL32(MXZMD), AMOL33(MXZMD),       FX06930
     *                AMOL34(MXZMD), AMOL35(MXZMD), AMOL36(MXZMD),              
     *                AMOL37(MXZMD), AMOL38(MXZMD)                              
C                                                                        FX06950
      DATA LAYXMX / 4000 /                                               FX06960
C                                                                        FX06970
      DATA ALTX /                                                        FX06980
     *       0.0,       1.0,       2.0,       3.0,       4.0,            FX06990
     *       5.0,       6.0,       7.0,       8.0,       9.0,            FX07000
     *      10.0,      11.0,      12.0,      13.0,      14.0,            FX07010
     *      15.0,      16.0,      17.0,      18.0,      19.0,            FX07020
     *      20.0,      21.0,      22.0,      23.0,      24.0,            FX07030
     *      25.0,      27.5,      30.0,      32.5,      35.0,            FX07040
     *      37.5,      40.0,      42.5,      45.0,      47.5,            FX07050
     *      50.0,      55.0,      60.0,      65.0,      70.0,            FX07060
     *      75.0,      80.0,      85.0,      90.0,      95.0,            FX07070
     *     100.0,     105.0,     110.0,     115.0,     120.0,            FX07080
     *     MXZ50*0.0/                                                    FX07090
C                                                                        FX07100
C     DATA AMOL1 / CLONO2 /                                              FX07110
C                                                                        FX07120
      DATA AMOL1 /                                                       FX07130
     *  4.737E-06, 4.162E-06, 3.587E-06, 2.891E-06, 2.195E-06,           FX07140
     *  1.717E-06, 1.238E-06, 9.775E-07, 7.170E-07, 6.231E-07,           FX07150
     *  5.292E-07, 5.313E-07, 5.334E-07, 1.451E-06, 2.368E-06,           FX07160
     *  1.037E-05, 1.837E-05, 5.571E-05, 9.304E-05, 1.748E-04,           FX07170
     *  2.566E-04, 3.681E-04, 4.796E-04, 5.910E-04, 7.024E-04,           FX07180
     *  7.724E-04, 8.587E-04, 7.428E-04, 4.585E-04, 2.005E-04,           FX07190
     *  5.867E-05, 8.818E-06, 1.319E-06, 1.610E-07, 1.889E-08,           FX07200
     *  1.855E-09, 7.032E-11, 2.870E-12, 2.174E-13, 3.025E-14,           FX07210
     *  3.257E-15, 2.634E-17, 3.313E-20, 2.134E-23, 1.366E-25,           FX07220
     *  4.128E-28, 3.433E-30, 0.       , 0.       , 0.       ,           FX07230
     *  MXZ50*0.0/                                                       FX07240
C                                                                        FX07250
C     DATA AMOL2 / HNO4 /                                                FX07260
C                                                                        FX07270
      DATA AMOL2 /                                                       FX07280
     *  8.851E-07, 2.031E-06, 3.177E-06, 7.444E-06, 1.171E-05,           FX07290
     *  1.962E-05, 2.752E-05, 3.284E-05, 3.816E-05, 3.576E-05,           FX07300
     *  3.336E-05, 2.928E-05, 2.519E-05, 2.814E-05, 3.109E-05,           FX07310
     *  4.406E-05, 5.703E-05, 8.127E-05, 1.055E-04, 1.344E-04,           FX07320
     *  1.632E-04, 1.946E-04, 2.260E-04, 2.541E-04, 2.822E-04,           FX07330
     *  3.010E-04, 3.016E-04, 2.175E-04, 1.151E-04, 4.626E-05,           FX07340
     *  1.448E-05, 3.333E-06, 8.857E-07, 2.244E-07, 5.992E-08,           FX07350
     *  1.669E-08, 2.490E-09, 3.659E-10, 7.913E-11, 3.168E-11,           FX07360
     *  7.075E-12, 2.676E-13, 3.296E-15, 1.497E-18, 2.227E-21,           FX07370
     *  1.918E-24, 2.507E-26, 3.283E-28, 1.605E-29, 2.026E-30,           FX07380
     *  MXZ50*0.0/                                                       FX07390
C                                                                        FX07400
C     DATA AMOL3 / CHCL2F /                                              FX07410
C                                                                        FX07420
      DATA AMOL3 /                                                       FX07430
     *  50*-99.                                              ,           FX07440
     *  MXZ50*0.0/                                                       FX07450
C                                                                        FX07460
C     DATA AMOL4 / CCL4 /                                                FX07470
C                                                                        FX07480
      DATA AMOL4 /                                                       FX07490
     *  1.300E-04, 1.300E-04, 1.299E-04, 1.299E-04, 1.298E-04,           FX07500
     *  1.297E-04, 1.296E-04, 1.295E-04, 1.294E-04, 1.293E-04,           FX07510
     *  1.292E-04, 1.289E-04, 1.285E-04, 1.266E-04, 1.247E-04,           FX07520
     *  1.187E-04, 1.127E-04, 1.026E-04, 9.256E-05, 8.037E-05,           FX07530
     *  6.817E-05, 5.611E-05, 4.405E-05, 3.395E-05, 2.385E-05,           FX07540
     *  1.701E-05, 5.027E-06, 8.202E-07, 1.204E-07, 1.304E-08,           FX07550
     *  1.050E-09, 4.864E-11, 5.081E-12, 5.372E-13, 5.548E-14,           FX07560
     *  5.688E-15, 2.281E-16, 5.092E-18, 1.699E-19, 3.184E-21,           FX07570
     *  9.600E-23, 1.638E-24, 4.605E-26, 6.985E-28, 1.743E-29,           FX07580
     *  2.224E-31, 4.283E-33, 0.       , 0.       , 0.       ,           FX07590
     *  MXZ50*0.0/                                                       FX07600
C                                                                        FX07610
C     DATA AMOL5 / CCL3F /                                               FX07620
C                                                                        FX07630
      DATA AMOL5 /                                                       FX07640
     *  1.400E-04, 1.400E-04, 1.399E-04, 1.399E-04, 1.398E-04,           FX07650
     *  1.397E-04, 1.396E-04, 1.396E-04, 1.395E-04, 1.394E-04,           FX07660
     *  1.392E-04, 1.389E-04, 1.386E-04, 1.368E-04, 1.349E-04,           FX07670
     *  1.292E-04, 1.234E-04, 1.138E-04, 1.041E-04, 9.216E-05,           FX07680
     *  8.021E-05, 6.799E-05, 5.576E-05, 4.480E-05, 3.384E-05,           FX07690
     *  2.550E-05, 9.634E-06, 2.441E-06, 5.553E-07, 1.024E-07,           FX07700
     *  1.581E-08, 1.939E-09, 3.811E-10, 7.716E-11, 1.585E-11,           FX07710
     *  3.658E-12, 4.173E-13, 3.465E-14, 3.353E-15, 2.383E-16,           FX07720
     *  2.084E-17, 1.346E-18, 1.080E-19, 6.099E-21, 4.246E-22,           FX07730
     *  1.923E-23, 1.110E-24, 5.158E-26, 3.393E-27, 3.738E-28,           FX07740
     *  MXZ50*0.0/                                                       FX07750
C                                                                        FX07760
C     DATA AMOL6 / CCL2F2 /                                              FX07770
C                                                                        FX07780
      DATA AMOL6 /                                                       FX07790
     *  2.400E-04, 2.400E-04, 2.399E-04, 2.399E-04, 2.398E-04,           FX07800
     *  2.398E-04, 2.397E-04, 2.396E-04, 2.395E-04, 2.394E-04,           FX07810
     *  2.393E-04, 2.390E-04, 2.387E-04, 2.370E-04, 2.353E-04,           FX07820
     *  2.300E-04, 2.247E-04, 2.157E-04, 2.066E-04, 1.952E-04,           FX07830
     *  1.838E-04, 1.712E-04, 1.585E-04, 1.452E-04, 1.319E-04,           FX07840
     *  1.183E-04, 8.552E-05, 5.683E-05, 3.498E-05, 2.013E-05,           FX07850
     *  1.111E-05, 6.014E-06, 3.446E-06, 1.998E-06, 1.181E-06,           FX07860
     *  7.687E-07, 3.876E-07, 1.818E-07, 8.265E-08, 3.432E-08,           FX07870
     *  1.380E-08, 4.984E-09, 1.704E-09, 4.917E-10, 1.272E-10,           FX07880
     *  2.351E-11, 3.640E-12, 4.251E-13, 4.981E-14, 8.792E-15,           FX07890
     *  MXZ50*0.0/                                                       FX07900
C                                                                        FX07910
C     DATA AMOL7 / C2CL2F4 /                                             FX07920
C                                                                        FX07930
      DATA AMOL7 /                                                       FX07940
     *  1.200E-05, 1.200E-05, 1.200E-05, 1.200E-05, 1.199E-05,           FX07950
     *  1.199E-05, 1.199E-05, 1.199E-05, 1.198E-05, 1.198E-05,           FX07960
     *  1.198E-05, 1.197E-05, 1.196E-05, 1.191E-05, 1.185E-05,           FX07970
     *  1.167E-05, 1.149E-05, 1.120E-05, 1.090E-05, 1.053E-05,           FX07980
     *  1.015E-05, 9.731E-06, 9.311E-06, 8.865E-06, 8.419E-06,           FX07990
     *  7.949E-06, 6.770E-06, 5.620E-06, 4.547E-06, 3.623E-06,           FX08000
     *  2.884E-06, 2.315E-06, 1.906E-06, 1.600E-06, 1.375E-06,           FX08010
     *  1.231E-06, 1.037E-06, 8.645E-07, 7.140E-07, 5.799E-07,           FX08020
     *  4.610E-07, 3.530E-07, 2.524E-07, 1.588E-07, 7.585E-08,           FX08030
     *  2.131E-08, 3.107E-09, 2.089E-10, 1.084E-11, 8.968E-13,           FX08040
     *  MXZ50*0.0/                                                       FX08050
C                                                                        FX08060
C     DATA AMOL8 / C2CL3F3 /                                             FX08070
C                                                                        FX08080
      DATA AMOL8 /                                                       FX08090
     *  1.900E-05, 1.900E-05, 1.899E-05, 1.899E-05, 1.898E-05,           FX08100
     *  1.898E-05, 1.897E-05, 1.896E-05, 1.895E-05, 1.894E-05,           FX08110
     *  1.893E-05, 1.890E-05, 1.887E-05, 1.871E-05, 1.854E-05,           FX08120
     *  1.803E-05, 1.751E-05, 1.664E-05, 1.576E-05, 1.466E-05,           FX08130
     *  1.356E-05, 1.236E-05, 1.116E-05, 9.931E-06, 8.702E-06,           FX08140
     *  7.515E-06, 4.787E-06, 2.678E-06, 1.366E-06, 6.370E-07,           FX08150
     *  2.820E-07, 1.222E-07, 5.930E-08, 2.949E-08, 1.507E-08,           FX08160
     *  8.617E-09, 3.550E-09, 1.304E-09, 4.610E-10, 1.427E-10,           FX08170
     *  4.322E-11, 1.131E-11, 2.861E-12, 5.798E-13, 1.059E-13,           FX08180
     *  1.182E-14, 9.372E-16, 3.545E-17, 1.223E-18, 6.979E-20,           FX08190
     *  MXZ50*0.0/                                                       FX08200
C                                                                        FX08210
C     DATA AMOL9 / N2O5 /                                                FX08220
C                                                                        FX08230
      DATA AMOL9 /                                                       FX08240
     *  1.312E-10, 4.065E-10, 6.818E-10, 5.329E-08, 1.059E-07,           FX08250
     *  1.177E-06, 2.248E-06, 2.435E-06, 2.622E-06, 2.526E-06,           FX08260
     *  2.430E-06, 2.714E-06, 2.998E-06, 7.354E-06, 1.171E-05,           FX08270
     *  3.638E-05, 6.105E-05, 1.157E-04, 1.703E-04, 2.471E-04,           FX08280
     *  3.239E-04, 4.204E-04, 5.168E-04, 6.318E-04, 7.468E-04,           FX08290
     *  8.576E-04, 9.888E-04, 7.845E-04, 4.140E-04, 1.556E-04,           FX08300
     *  4.229E-05, 7.489E-06, 1.426E-06, 2.195E-07, 3.706E-08,           FX08310
     *  6.586E-09, 4.858E-10, 7.919E-12, 1.913E-13, 2.626E-15,           FX08320
     *  3.692E-17, 5.125E-19, 2.169E-21, 6.096E-25, 6.336E-28,           FX08330
     *  9.855E-32, 0.       , 0.       , 0.       , 0.       ,           FX08340
     *  MXZ50*0.0/                                                       FX08350
C                                                                        FX08360
C     DATA AMOL10 / HNO3 /                                               FX08370
C                                                                        FX08380
      DATA AMOL10 /                                                      FX08390
     *  5.738E-05, 6.671E-05, 7.603E-05, 8.176E-05, 8.748E-05,           FX08400
     *  9.153E-05, 9.558E-05, 9.914E-05, 1.027E-04, 1.111E-04,           FX08410
     *  1.195E-04, 1.431E-04, 1.667E-04, 3.217E-04, 4.766E-04,           FX08420
     *  9.273E-04, 1.378E-03, 2.070E-03, 2.762E-03, 3.514E-03,           FX08430
     *  4.266E-03, 4.891E-03, 5.516E-03, 5.858E-03, 6.200E-03,           FX08440
     *  6.170E-03, 5.684E-03, 4.611E-03, 3.245E-03, 1.978E-03,           FX08450
     *  1.015E-03, 3.855E-04, 1.252E-04, 3.480E-05, 9.533E-06,           FX08460
     *  2.792E-06, 5.898E-07, 1.885E-07, 4.912E-08, 1.021E-08,           FX08470
     *  2.233E-09, 1.122E-09, 3.566E-11, 3.213E-14, 7.770E-17,           FX08480
     *  9.752E-20, 1.129E-21, 2.151E-23, 1.720E-24, 2.813E-25,           FX08490
     *  MXZ50*0.0/                                                       FX08500
C                                                                        FX08510
C     DATA AMOL11 / CF4 /                                                FX08520
C                                                                        FX08530
      DATA AMOL11 /                                                      FX08540
     *  50*-99.                                              ,           FX08550
     *  MXZ50*0.0/                                                       FX08560
C                                                                        FX08570
C     DATA AMOL12 / CHCLF2 /  ????                                       FX08580
C                                                                        FX08590
      DATA AMOL12 /                                                      FX08600
     *  6.000E-05, 5.994E-05, 5.987E-05, 5.982E-05, 5.977E-05,           FX08610
     *  5.974E-05, 5.970E-05, 5.968E-05, 5.966E-05, 5.963E-05,           FX08620
     *  5.960E-05, 5.955E-05, 5.949E-05, 5.921E-05, 5.893E-05,           FX08630
     *  5.808E-05, 5.723E-05, 5.582E-05, 5.441E-05, 5.265E-05,           FX08640
     *  5.089E-05, 4.897E-05, 4.705E-05, 4.502E-05, 4.298E-05,           FX08650
     *  4.084E-05, 3.548E-05, 3.021E-05, 2.514E-05, 2.062E-05,           FX08660
     *  1.686E-05, 1.392E-05, 1.184E-05, 1.036E-05, 9.356E-06,           FX08670
     *  8.784E-06, 8.163E-06, 7.741E-06, 7.449E-06, 7.201E-06,           FX08680
     *  6.919E-06, 6.524E-06, 5.872E-06, 4.867E-06, 3.396E-06,           FX08690
     *  1.808E-06, 6.935E-07, 2.066E-07, 5.485E-08, 1.930E-08,           FX08700
     *  MXZ50*0.0/                                                       FX08710
C                                                                        FX08720
C     DATA AMOL13 / ?????? /                                             FX08730
C                                                                        FX08740
      DATA AMOL13 /                                                      FX08750
     *  50*-99.                                              ,           FX08760
     *  MXZ50*0.0/                                                       FX08770
C                                                                        FX08780
C     DATA AMOL14 / ?????? /                                             FX08790
C                                                                        FX08800
      DATA AMOL14 /                                                      FX08810
     *  50*-99.                                              ,           FX08820
     *  MXZ50*0.0/                                                       FX08830
C                                                                        FX08840
C     DATA AMOL15 / ?????? /                                             FX08850
C                                                                        FX08860
      DATA AMOL15 /                                                      FX08870
     *  50*-99.                                              ,           FX08880
     *  MXZ50*0.0/                                                       FX08890
C                                                                        FX08900
C     DATA AMOL16 / ?????? /                                             FX08910
C                                                                        FX08920
      DATA AMOL16 /                                                      FX08930
     *  50*-99.                                              ,           FX08940
     *  MXZ50*0.0/                                                       FX08950
C                                                                        FX08960
C     DATA AMOL17 / ?????? /                                             FX08970
C                                                                        FX08980
      DATA AMOL17 /                                                      FX08990
     *  50*-99.                                              ,           FX09000
     *  MXZ50*0.0/                                                       FX09010
C                                                                        FX09020
C     DATA AMOL18 / ?????? /                                             FX09030
C                                                                        FX09040
      DATA AMOL18 /                                                      FX09050
     *  50*-99.                                              ,           FX09060
     *  MXZ50*0.0/                                                       FX09070
C                                                                        FX09080
C     DATA AMOL19 / ?????? /                                             FX09090
C                                                                        FX09100
      DATA AMOL19 /                                                      FX09110
     *  50*-99.                                              ,           FX09120
     *  MXZ50*0.0/                                                       FX09130
C                                                                        FX09140
C     DATA AMOL20 / ?????? /                                             FX09150
C                                                                        FX09160
      DATA AMOL20 /                                                      FX09170
     *  50*-99.                                              ,           FX09180
     *  MXZ50*0.0/                                                       FX09190
C                                                                        FX09200
C     DATA AMOL21 / ?????? /                                             FX09210
C                                                                        FX09220
      DATA AMOL21 /                                                      FX09230
     *  50*-99.                                              ,           FX09240
     *  MXZ50*0.0/                                                       FX09250
C                                                                        FX09260
C     DATA AMOL22 / ?????? /                                             FX09270
C                                                                        FX09280
      DATA AMOL22 /                                                      FX09290
     *  50*-99.                                              ,           FX09300
     *  MXZ50*0.0/                                                       FX09310
C                                                                        FX09320
C     DATA AMOL23 / ?????? /                                             FX09330
C                                                                        FX09340
      DATA AMOL23 /                                                      FX09350
     *  50*-99.                                              ,           FX09360
     *  MXZ50*0.0/                                                       FX09370
C                                                                        FX09380
C     DATA AMOL24 / ?????? /                                             FX09390
C                                                                        FX09400
      DATA AMOL24 /                                                      FX09410
     *  50*-99.                                              ,           FX09420
     *  MXZ50*0.0/                                                       FX09430
C                                                                        FX09440
C     DATA AMOL25 / ?????? /                                             FX09450
C                                                                        FX09460
      DATA AMOL25 /                                                      FX09470
     *  50*-99.                                              ,           FX09480
     *  MXZ50*0.0/                                                       FX09490
C                                                                        FX09500
C     DATA AMOL26 / ?????? /                                             FX09510
C                                                                        FX09520
      DATA AMOL26 /                                                      FX09530
     *  50*-99.                                              ,           FX09540
     *  MXZ50*0.0/                                                       FX09550
C                                                                        FX09560
C     DATA AMOL27 / ?????? /                                             FX09570
C                                                                        FX09580
      DATA AMOL27 /                                                      FX09590
     *  50*-99.                                              ,           FX09600
     *  MXZ50*0.0/                                                       FX09610
C                                                                        FX09620
C     DATA AMOL28 / ?????? /                                             FX09630
C                                                                        FX09640
      DATA AMOL28 /                                                      FX09650
     *  50*-99.                                              ,           FX09660
     *  MXZ50*0.0/                                                       FX09670
C                                                                        FX09680
C     DATA AMOL29 / ?????? /                                             FX09690
C                                                                        FX09700
      DATA AMOL29 /                                                      FX09710
     *  50*-99.                                              ,           FX09720
     *  MXZ50*0.0/                                                       FX09730
C                                                                        FX09740
C     DATA AMOL30 / ?????? /                                             FX09750
C                                                                        FX09760
      DATA AMOL30 /                                                      FX09770
     *  50*-99.                                              ,           FX09780
     *  MXZ50*0.0/                                                       FX09790
C                                                                        FX09800
C     DATA AMOL31 / ?????? /                                             FX09810
C                                                                        FX09820
      DATA AMOL31 /                                                      FX09830
     *  50*-99.                                              ,           FX09840
     *  MXZ50*0.0/                                                       FX09850
C                                                                        FX09860
C     DATA AMOL32 / ?????? /                                             FX09870
C                                                                        FX09880
      DATA AMOL32 /                                                      FX09890
     *  50*-99.                                              ,           FX09900
     *  MXZ50*0.0/                                                       FX09910
C                                                                        FX09920
C     DATA AMOL33 / ?????? /                                             FX09930
C                                                                        FX09940
      DATA AMOL33 /                                                      FX09950
     *  50*-99.                                              ,           FX09960
     *  MXZ50*0.0/                                                       FX09970
C                                                                        FX09980
C     DATA AMOL34 / ?????? /                                             FX09990
C                                                                        FX10000
      DATA AMOL34 /                                                      FX10010
     *  50*-99.                                              ,           FX10020
     *  MXZ50*0.0/                                                       FX10030
C                                                                        FX10040
C     DATA AMOL35 / ?????? /                                             FX10050
C                                                                        FX10060
      DATA AMOL35 /                                                      FX10070
     *  50*-99.                                              ,           FX10080
     *  MXZ50*0.0/                                                       FX10090
C                                                                        FX10100
C     DATA AMOL36 / ?????? /                                             FX10050
C                                                                        FX10060
      DATA AMOL36 /                                                      FX10070
     *  50*-99.                                              ,           FX10080
     *  MXZ50*0.0/                                                       FX10090
C                                                                        FX10100
C     DATA AMOL37 / ?????? /                                             FX10050
C                                                                        FX10060
      DATA AMOL37 /                                                      FX10070
     *  50*-99.                                              ,           FX10080
     *  MXZ50*0.0/                                                       FX10090
C                                                                        FX10100
C     DATA AMOL38 / ?????? /                                             FX10050
C                                                                        FX10060
      DATA AMOL38 /                                                      FX10070
     *  50*-99.                                              ,           FX10080
     *  MXZ50*0.0/                                                       FX10090
C                                                                        FX10100
      END                                                                FX10110
C                                                                               
C -------------------------------------------------------------------           
C                                                                               
      SUBROUTINE NEWH2(H1,H2,ANGLE,RANGE,BETA,LEN,HTAN,PHI)                     
C                                                                               
C     Changed for LBLRTM to correct geometry problems                           
C                                                                               
C     THIS ROUTINE DETERMINES H2,BETA, TANGENT HEIGHT AND LEN.                  
C     ADOPTED FROM THE MODTRAN2 GEOMETRY PACKAGE                                
C                                                                               
C     INPUTS ARE: H1, ZENTIH ANGLE (ANGLE) AND RANGE.                           
C     LEN = 1 IF THE PATH GOES THROUGH HTAN.                                    
C                                                                               
C     MXFSC IS THE MAXIMUM NUMBER OF LAYERS FOR OUTPUT TO FASE01                
C     MXLAY IS THE MAXIMUM NUMBER OF OUTPUT LAYERS                              
C     MXZMD IS THE MAX NUMBER OF LEVELS IN THE ATMOSPHERIC PROFILE              
C         STORED IN ZMDL (INPUT)                                                
C     MXPDIM IS THE MAXIMUM NUMBER OF LEVELS IN THE PROFILE ZPTH                
C         OBTAINED BY MERGING ZMDL AND ZOUT                                     
C     MXMOL IS THE MAXIMUM NUMBER OF MOLECULES, KMXNOM IS THE DEFAULT           
C                                                                               
      PARAMETER (MXFSC=200,MXLAY=MXFSC+3,MXZMD=4000,                            
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)            
C                                                                               
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,               
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,              
     *              NLTEFL,LNFIL4,LNGTH4                                        
C                                                                               
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,                    
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                             
C                                                                               
C     BLANK COMMON FOR ZMDL                                                     
C                                                                               
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)             
      COMMON WPATH(IM2,16),TBBY(IM2)                                            
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)                
C                                                                               
      CHARACTER*8      HMOD                                                     
C                                                                               
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                               
      REAL*8           CPATH,CPJ,CPJ1,SH,GAMMA,ANDEXD,CRFRCT,RE2                
C                                                                               
      CRFRCT(H)=(RE2+H)*ANDEXD(H,SH,GAMMA)                                      
C                                                                               
      RE2=RE                                                                    
C     COMPUTE CPATH OR PATH CONSTANT                                            
      CALL FNDSHD(H1,SH,GAMMA)                                                  
      CPATH = CRFRCT(H1)*SIN(ANGLE/DEG)                                         
C                                                                               
C     ANGLE = 90 at H1 implies that H1 = tangent height                         
C                                                                               
      IF (ANGLE.EQ.90.0) THEN                                                   
          HTAN=H1                                                               
      ELSE                                                                      
          DO 100 J=1,IMMAX                                                      
              IF (H1.GE.ZMDL(J)) JMAX=J                                         
  100     CONTINUE                                                              
          JMAX=JMAX+1                                                           
          ZJ1=ZMDL(JMAX)                                                        
          CPJ1=CRFRCT(ZJ1)                                                      
          HTAN=-1.0                                                             
          DO 200 J=JMAX,1,-1                                                    
              IF (HTAN.LT.0.0) THEN                                             
                  IF (J.EQ.1) THEN                                              
                      HTAN=0.0                                                  
                  ELSE                                                          
                      CPJ=CPJ1                                                  
                      ZJ=ZJ1                                                    
                      ZJ1=ZMDL(J-1)                                             
                      CPJ1=CRFRCT(ZJ1)                                          
                      IF ((CPATH.LE.CPJ).AND.(CPATH.GE.CPJ1)) THEN              
                          HTAN=RTBIS(ZJ1,CPJ1,ZJ,CPJ,CPATH)                     
                      ENDIF                                                     
                  ENDIF                                                         
              ENDIF                                                             
  200     CONTINUE                                                              
      ENDIF                                                                     
C                                                                               
C     Find H2, BETA AND LEN                                                     
C                                                                               
      CALL FNDPTH(CPATH,H1,HTAN,H2,RANGE,BETA,LEN,ANGLE,PHI)                    
C                                                                               
C     Ensure LEN is not reset in FSCGEO if direct path                          
      IF (LEN.EQ.0) HTAN=H2                                                     
C                                                                               
C     IF (ANGLE .LE. 90.0) HTAN CARRIES HMIN NOT HTAN                           
      IF (ANGLE .LE. 90.0) HTAN = MIN(H1,H2)                                    
C                                                                               
      RETURN                                                                    
C                                                                               
      END                                                                       
C                                                                               
C ----------------------------------------------------------------              
C                                                                               
      FUNCTION RTBIS(X1,CX1,X2,CX2,CPATH)                                       
C                                                                               
C     THIS FUNCTION FINDS THE ROOT OF                                           
C            FUNC(X) = X*REFRACTIVE INDEX - CPA                                 
C                                                                               
C     THE ROOT IS ACTUALLY THE TANGENT HEIGHT, BETWEEN X1 AND X2.               
C     THIS ROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS, ET AL.                 
C                                                                               
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,                    
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                             
                                                                                
      REAL*8           CX1,CX2,CPATH,F,FMID,SH,GAMMA,ANDEXD                     
      DATA XACC/1E-5/                                                           
      PARAMETER (JMAX=40)                                                       
C                                                                               
      FMID=CX2-CPATH                                                            
      F=CX1-CPATH                                                               
      IF(F*FMID.GE.0.) STOP 'ROOT MUST BE BRACKETED FOR BISECTION.'             
      IF(F.LT.0.)THEN                                                           
         RTBIS=X1                                                               
         DX=X2-X1                                                               
      ELSE                                                                      
         RTBIS=X2                                                               
         DX=X1-X2                                                               
      ENDIF                                                                     
      DO 11 J=1,JMAX                                                            
         DX=DX*.5                                                               
         XMID=RTBIS+DX                                                          
         CALL FNDSHD(XMID,SH,GAMMA)                                             
         FMID=ANDEXD(XMID,SH,GAMMA)*(XMID+RE)-CPATH                             
         IF(FMID.LE.0.)RTBIS=XMID                                               
         IF(ABS(DX).LT.XACC .OR. FMID.EQ.0.) RETURN                             
 11   CONTINUE                                                                  
C                                                                               
C     COMES HERE IF UNABLE TO SOLVE.                                            
C                                                                               
      IF (ABS(CX2) .LT. ABS(CX1)) THEN                                          
         RTBIS = X2                                                             
      ELSE                                                                      
         RTBIS = X1                                                             
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
C                                                                               
C ----------------------------------------------------------------              
C                                                                               
      SUBROUTINE FNDPTH(CPATH,H1,HTAN,H2,RANGEI,BETA,LEN,ANGLE,PHI)             
C                                                                               
C     THIS ROUTINE DETERMINES H2, BETA AND LEN.                                 
C     INPUTS ARE H1, HTAN (TANGENT HEIGHT), RANGE (RANGEI) AND                  
C     THE PATH CONSTANT, CPATH.                                                 
C     RANGEO IS THE OUTPUT RANGE WHICH SHOULD EQUAL THE INPUT RANGE.            
C                                                                               
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,                    
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                             
C                                                                               
      REAL*8           SAVE,STHETA,CAPRJ,PNTGRN,CTHETA,CTHET1,DX,               
     *     DRNG,DBETA,R,DIFF,CPATH,ANDEXD,SH,GAMMA,RX,RATIO,RPLDR               
C                                                                               
      DATA DR/0.005/                                                            
C                                                                               
      IF (RANGEI .LT. DR) STOP'STOPPED IN FNDPTH'                               
C                                                                               
C     (RANGEI .LT. DR) SHOULD NOT HAPPEN; SO THIS CHECK IS REDUNDANT.           
C                                                                               
      RANGEO = 0                                                                
      BETA = 0                                                                  
      DO 200 I = 1, 2                                                           
C                                                                               
         IF (ANGLE .LE. 90.0000 .AND. I .EQ. 1) GO TO 200                       
C                                                                               
C        IF (ANGLE .LE. 90.0000) THE PATH DOES NOT GO THROUGH HTAN.             
C        IF (ANGLE .LE. 90.0000) THE I = 1 CALCULATION SHOULD NOT BE DONE       
C        IF (ANGLE .LE. 90.0000) FOR I = 2, R1 = H1                             
C                                                                               
         IF (I .EQ. 1) THEN                                                     
            R1 = H1                                                             
            R2 = HTAN                                                           
         ELSEIF (I .EQ. 2) THEN                                                 
            IF (HTAN .LT. 0.001 .AND. ANGLE .GT. 90) GO TO 200                  
C                                                                               
C           IF (HTAN APPROXIMATELY 0) THEN YOU ARE ABOUT TO HIT THE EARTH       
C                                                                               
            R2 = ZMAX                                                           
            IF (ANGLE .LE. 90.0000) THEN                                        
               R1 = H1                                                          
            ELSE                                                                
               R1 =HTAN                                                         
            ENDIF                                                               
         ENDIF                                                                  
         IF (R2 .LT. R1) THEN                                                   
            DZ = -DR                                                            
         ELSE                                                                   
            DZ = DR                                                             
         ENDIF                                                                  
c                                                                               
                                                                                
         z = r1                                                                 
         DO 100 while (z.lt.r2)                                                 
            Z2=Z                                                                
            R=Z+RE                                                              
            CALL FNDSHD(Z2,SH,GAMMA)                                            
            RX=ANDEXD(Z2,SH,GAMMA)                                              
            STHETA = CPATH/(RX*R)                                               
            IF (STHETA .GT. 1.0) STHETA = 1.                                    
            IF (STHETA .LT.-1.0) STHETA =-1.                                    
            SAVE = STHETA                                                       
            CTHETA = SQRT(1.0-STHETA**2)                                        
            IF (R1 .GT. R2) CTHETA = -CTHETA                                    
C                                                                               
C           IF (R1 .GT. R2) THEN CTHETA IS NEGATIVE BECAUSE THETA .GT. 90       
C                                                                               
            RATIO=-(RX*SH)/(RX-1.0)                                             
            CAPRJ = -R/RATIO                                                    
            PNTGRN = 1.0/(1.0-CAPRJ*STHETA*STHETA)                              
            RPLDR = R+DZ                                                        
            Z2 = Z+DZ                                                           
            CALL FNDSHD(Z2,SH,GAMMA)                                            
            RX=ANDEXD(Z2,SH,GAMMA)                                              
            STHETA = CPATH/(RX*RPLDR)                                           
            CTHET1 = CTHETA                                                     
            CTHETA = SQRT(1.0-STHETA**2)                                        
            IF (R1 .GT. R2) CTHETA = -CTHETA                                    
            DX=CTHETA*DZ+(CTHETA-CTHET1)*R                                      
            DRNG = PNTGRN*DX                                                    
            RANGEO = RANGEO + DRNG                                              
C                                                                               
            DBETA = (((SAVE+STHETA)*0.5) * (PNTGRN*DX)) /                       
     *              (Z-0.5*DZ+RE)                                               
            BETA = BETA+DBETA                                                   
            IF (RANGEO .GE. RANGEI) THEN                                        
               DIFF = (RANGEI-(RANGEO-DRNG))                                    
               H2 = Z + (DZ/DRNG)*DIFF                                          
               BETA = BETA*DEG                                                  
               IF (I .EQ. 2) THEN                                               
                  LEN = 1                                                       
                  IF (ANGLE .LE. 90.0000) LEN = 0                               
                  IF (H2 .LT. HTAN) THEN                                        
C                                                                               
C                    THIS WILL BE THE CASE IF I = 2, AND YOU HAVE               
C                    GONE THROUGH THE R-LOOP BARELY (ONLY) ONCE.                
C                                                                               
                     H2 = HTAN                                                  
                     LEN = 0                                                    
                  ENDIF                                                         
               ELSE                                                             
                  LEN = 0                                                       
               ENDIF                                                            
C                                                                               
C              CORRECTION FOR VERY SHORT PATHS; HERE IT IS ABOUT 5 KM           
C                                                                               
               IF (RANGEI .LT. 5.0 .AND. RANGEO/RANGEI .GT. 1.05) THEN          
C                                                                               
C                 CALCULATE BETA BY STARIGHT LINE GEOMETRY.                     
C                                                                               
                  PERP  = SIN(ANGLE/DEG)*RANGEI                                 
                  BASE = COS(ANGLE/DEG)*RANGEI + RE+H1                          
                  BETA = ATAN(PERP/BASE)*DEG                                    
                  RANGEO = RANGEI                                               
C                                                                               
C                 H2 = BASE - RE                                                
C                                                                               
                  H2 = COS(ANGLE/DEG)*RANGEI+H1                                 
               ENDIF                                                            
               PHI = 180.0 - ACOS(CTHETA)*DEG                                   
               RETURN                                                           
            ENDIF                                                               
            z=z+dz                                                              
 100     CONTINUE                                                               
 200  CONTINUE                                                                  
C                                                                               
C     COMES HERE IF YOU HAVE REACHED ZMAX, BUT YOUR RANGEI IS STILL             
C     NOT EQUAL TO OUTPUT VALUE.                                                
C     IN THIS CASE DO THE FOLLOWING.                                            
C                                                                               
      RANGEI = RANGEO                                                           
      H2 = ZMAX                                                                 
      IF (ANGLE .LE. 90) THEN                                                   
         LEN = 0                                                                
      ELSE                                                                      
         LEN = 1                                                                
      ENDIF                                                                     
      IF (HTAN .LT. 0.001 .AND. ANGLE .GT. 90) THEN                             
C                                                                               
C        YOU HAVE HIT THE EARTH IF YOU ARE AT THIS POINT OF THE CODE            
C                                                                               
         LEN = 0                                                                
         H2 = 0                                                                 
      ENDIF                                                                     
      BETA = BETA*DEG                                                           
      PHI = 180.0 - ACOS(CTHETA)*DEG                                            
C                                                                               
      RETURN                                                                    
      END                                                                       
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE FNDSHD (H,SH,GAMMA)                                            
C                                                                               
C     Double precision version of FINDSH - needed for improved geometry         
C                                                                               
C     *****************************************************************         
C     GIVEN AN ALTITUDE H, THIS SUBROUTINE FINDS THE LAYER BOUNDARIES           
C     Z(I1) AND Z(I2) WHICH CONTAIN H,  THEN CALCULATES THE SCALE               
C     HEIGHT (SH) AND THE VALUE AT THE GROUND (GAMMA+1) FOR THE                 
C     REFRACTIVITY (INDEX OF REFRACTION -1)                                     
C     *****************************************************************         
C                                                                               
      PARAMETER (MXFSC=200,MXLAY=MXFSC+3,MXZMD=4000,                            
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)            
C                                                                               
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,               
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,              
     *              NLTEFL,LNFIL4,LNGTH4                                        
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,                    
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                             
      COMMON RELHUM(MXZMD),HSTOR(MXZMD),ICH(4),AVH(16),TX(16),W(16)             
      COMMON WPATH(IM2,16),TBBY(IM2)                                            
      COMMON ABSC(5,47),EXTC(5,47),ASYM(5,47),AVX2(47),AWCCON(5)                
C                                                                               
      REAL*8                SH,GAMMA                                            
      CHARACTER*8      HMOD                                                     
C                                                                               
      COMMON /CMN/HMOD(3),ZMDL(MXZMD),PM(MXZMD),TM(MXZMD),RFNDXM(MXZMD),        
     *       ZPTH(IM2),PP(IM2),TP(IM2),RFNDXP(IM2),SP(IM2),PPSUM(IM2),          
     *       TPSUM(IM2),RHOPSM(IM2),IMLOW,WGM(MXZMD),DENW(MXZMD),               
     *       AMTP(MXMOL,MXPDIM)                                                 
C                                                                               
      DO 10 IM = 2, IMMAX                                                       
         I2 = IM                                                                
         IF (ZMDL(IM).GE.H) GO TO 20                                            
   10 CONTINUE                                                                  
      I2 = IMMAX                                                                
   20 CONTINUE                                                                  
      I1 = I2-1                                                                 
      CALL SCLHTD (ZMDL(I1),ZMDL(I2),RFNDXM(I1),RFNDXM(I2),SH,GAMMA)            
C                                                                               
      RETURN                                                                    
C                                                                               
      END                                                                       
C                                                                               
C     ----------------------------------------------------------------          
C                                                                               
      SUBROUTINE SCLHTD (Z1,Z2,RFNDX1,RFNDX2,SH,GAMMA)                          
C                                                                               
C     Double precision version of SCALHT - needed for improved geometry         
C                                                                               
C     *****************************************************************         
C     THIS SUBROUTINE CALCULATES THE SCALE HEIGHT SH OF THE (INDEX OF           
C     REFRACTION-1.0) FROM THE VALUES OF THE INDEX AT THE ALTITUDES Z1          
C     AND Z2 ( Z1 < Z2). IT ALSO CALCULATES THE EXTRAPOLATED VALUE              
C     GAMMA OF THE (INDEX-1.0) AT Z = 0.0                                       
C     *****************************************************************         
C                                                                               
      REAL*8           SH,GAMMA                                                 
C                                                                               
      RF1 = RFNDX1+1.0E-20                                                      
      RF2 = RFNDX2+1.0E-20                                                      
      RATIO = RF1/RF2                                                           
      IF (ABS(RATIO-1.0).LT.1.0E-05) GO TO 10                                   
      SH = (Z2-Z1)/ LOG(RATIO)                                                  
      GAMMA = RF1*(RF2/RF1)**(-Z1/(Z2-Z1))                                      
      GO TO 20                                                                  
   10 CONTINUE                                                                  
C                                                                               
C     THE VARIATION IN THE INDEX OF REFRACTION WITH HEIGHT IS                   
C     INSIGNIFICANT OR ZERO                                                     
C                                                                               
      SH = 0.0                                                                  
      GAMMA = RFNDX1                                                            
   20 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
C                                                                               
      END                                                                       
C                                                                               
C ----------------------------------------------------------------              
C                                                                               
      FUNCTION ANDEXD (H,SH,GAMMA)                                              
C                                                                               
C     Double precision version of ANDEX - needed for improved geometry          
C                                                                               
C     *****************************************************************         
C     COMPUTES THE INDEX OF REFRACTION AT HEIGHT H, SH IS THE                   
C     SCALE HEIGHT, GAMMA IS THE VALUE AT H=0 OF THE REFRACTIVITY =             
C     INDEX-1                                                                   
C     *****************************************************************         
C                                                                               
      REAL*8     andexd, SH,GAMMA                                               
C                                                                               
      IF (SH.EQ.0.0) THEN                                                       
         ANDEXD = 1.0+GAMMA                                                     
      ELSE                                                                      
         ANDEXD = 1.0+GAMMA*EXP(-H/SH)                                          
      ENDIF                                                                     
C                                                                               
      RETURN                                                                    
C                                                                               
      END                                                                       
C                                                                               
C ----------------------------------------------------------------              
C                                                                               
      FUNCTION RADRFD (H,SH,GAMMA)                                              
C                                                                               
C     Double precision version of RADREF - needed for improved geometry         
C                                                                               
C     *****************************************************************         
C     COMPUTES THE RADIUS OF CURVATURE OF THE REFRACTED RAY FOR                 
C     A HORIZONTAL PATH.  RADREF = ANDEX/ D(ANDEX)/D(RADIUS)                    
C     *****************************************************************         
C                                                                               
      REAL*8     radrfd, SH,GAMMA,BIGNUM                                        
      DATA BIGNUM / 1.0D36 /                                                    
C                                                                               
      IF (SH.EQ.0.0) GO TO 10                                                   
      RADRFD = SH*(1.0+EXP(H/SH)/GAMMA)                                         
C                                                                               
      RETURN                                                                    
C                                                                               
   10 RADRFD = BIGNUM                                                           
C                                                                               
      RETURN                                                                    
C                                                                               
      END                                                                       
C                                                                               
                                                                                
      SUBROUTINE CMPALT(ILVL,PM,TM,DENW,REF_Z,REF_LAT,ZMDL)                     
                                                                                
C**************************************************************                 
C     AUTHOR: TONY CLOUGH, JENNIFER DELAMERE, JOHN WARDEN                       
C             JANUARY 2001                                                      
C     PROGRAM TO CALCULATE ALTITUDE LEVEL (ZMDL) GIVEN                          
C     PRESSURE (PM), TEMPERATURE (TM) AND THE NUMBER DENSITY                    
C     OF WATER VAPOR (DENW) USING THE HYDROSTATIC EQUATION                      
C                                                                               
C     INPUT:                                                                    
C      A) PRESSURE (MBAR)                                                       
C      B) TEMPERATURE (KELVIN)                                                  
C      C) NUMBER DENSITY OF WATER VAPOR                                         
C                                                                               
C     OUTPUT:                                                                   
C      A) ALTITUDE (KM)                                                         
C      IDEAL GAS LAW: CRIDOR (1996)                                             
C**************************************************************                 
                                                                                
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=4000,                           
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)            
                                                                                
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,        FA00880
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,       FA00890
     *              NLTEFL,LNFIL4,LNGTH4                                 FA00900
                                                                                
      COMMON /PARMTR/ DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,               
     *                IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,             FA00920
     *                IPHMID,IPDIM,KDIM,KMXNOM,NMOL                             
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,              
     *                RADCN1,RADCN2                                             
C                                                                               
      REAL PM(MXZMD),TM(MXZMD),DENW(MXZMD),ZMDL(MXZMD)                          
      REAL H2O_MIXRAT(MXZMD),COMP_FACTOR(MXZMD),ZTEMP(MXZMD)                    
                                                                                
      REAL Y                                                                    
      REAL CHI0                                                                 
      REAL T0,DT                                                                
      REAL C1,C2,C3                                                             
      REAL A, B, ALPHA                                                          
      REAL BTZ                                                                  
      REAL XINT_TOT                                                             
                                                                                
      DATA CA0/1.58123E-6/,CA1/-2.9331E-8/,CA2/1.1043E-10/                      
      DATA CB0/5.707E-6/,CB1/-2.051E-8/                                         
      DATA CC0/1.9898E-4/,CC1/-2.376E-6/                                        
      DATA CD/1.83E-11/,CE/-0.0765E-8/                                          
                                                                                
      DATA XMASS_H2O/0.018015/,XMASS_DRY/0.0289654/                             
                                                                                
      DATA RGAS/ 8.31441 /, BTZ/ 1.380662E-23 /                                 
                                                                                
C CALCULATE GRAVITY AT REFERENCE LATITUDE AT SURFACE                            
                                                                                
      G0 = 9.80612 - 0.02586*COS(2.0*PI*REF_LAT/180.0)                          
                                                                                
C CALCULATE THE NUMBER DENSITY OF TOTAL AIR MOLECULES [MOLEC/CM^3]              
C CALCULATE THE COMPRESSIBILITY FACTOR (COMP_FAC) FOR THE                       
C IDEAL GAS LAW                                                                 
      XMASS_RATIO = XMASS_H2O/XMASS_DRY                                         
      DO 10 J=1,ILVL                                                            
         DT = TM(J) - 273.15                                                    
         TOTAL_AIR = PM(J)*1.0E-4/(BTZ*TM(J))                                   
         DRY_AIR = TOTAL_AIR - DENW(J)                                          
         H2O_MIXRAT(J) = DENW(J)/DRY_AIR                                        
         CHIM = XMASS_RATIO*H2O_MIXRAT(J)                                       
         COMP_FACTOR(J) = 1. - (PM(J)*100/TM(J))*                               
     *        (CA0 + CA1*DT + CA2*DT**2 +                                       
     *        (CB0 + CB1*DT)*CHIM + (CC0 + CC1*DT)*CHIM**2) +                   
     *        (CD + CE*CHIM**2)*(PM(J)*100./TM(J))**2                           
 10   CONTINUE                                                                  
                                                                                
C CONVERT REFERENCE ALTITUDE TO METERS                                          
                                                                                
      ZTEMP(1) = REF_Z*1000.0                                                   
      ZMDL(1) = REF_Z                                                           
                                                                                
      DO 20 I=1, ILVL - 1                                                       
         GAVE = G0*(RE/(RE+ZTEMP(I)/1000.0))**2                                 
         Y =  LOG(PM(I+1)/PM(I))                                                
                                                                                
         IF (Y. NE. 0.0) THEN                                                   
            CHI0 = H2O_MIXRAT(I)                                                
            DCHI = (H2O_MIXRAT(I+1)-H2O_MIXRAT(I))/Y                            
                                                                                
            T0 = TM(I)                                                          
            DT = (TM(I+1) - TM(I))/Y                                            
                                                                                
            C1 = T0 + T0*CHI0                                                   
            C2 = T0*DCHI + DT*CHI0 + DT                                         
            C3 = DT*DCHI                                                        
                                                                                
            B = 1 + XMASS_RATIO*CHI0                                            
            A = XMASS_RATIO*DCHI                                                
            ALPHA = A/B                                                         
                                                                                
            IF ( ABS(ALPHA*Y) .GE. 0.01) THEN                                   
               PRINT*,'LAYER TOO THICK'                                         
               STOP                                                             
            ENDIF                                                               
                                                                                
            XINT_TOT = C1*Y + 0.5*(C2-C1*ALPHA)*Y**2 +                          
     &           0.3333*(C3-C2*ALPHA+C1*ALPHA**2)*Y**3                          
            XINT_TOT =  -XINT_TOT*RGAS/(XMASS_DRY*GAVE*B)                       
                                                                                
            ZTEMP(I+1) = ZTEMP(I) + XINT_TOT*COMP_FACTOR(I)                     
            ZMDL(I+1) = ZTEMP(I+1)/1000.                                        
        ELSE                                                                    
           ZTEMP(I+1) = ZMDL(I)*1000.0                                          
           ZMDL(I+1) = ZMDL(I)                                                  
        ENDIF                                                                   
 20      CONTINUE                                                               
                                                                                
         RETURN                                                                 
                                                                                
      END                                                                       
                                                                                
                                                                                
                                                                                
