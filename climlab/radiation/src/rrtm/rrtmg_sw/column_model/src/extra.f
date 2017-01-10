C     path:      $Source: /storm/rc1/cvsroot/rc/rrtm_sw/src/extra.f,v $
C     author:    $Author: jdelamer $
C     revision:  $Revision: 2.4 $
C     created:   $Date: 2005/07/07 20:28:52 $
      SUBROUTINE XSREAD (XV1,XV2)                                         E00010
C                                                                         E00020
      IMPLICIT DOUBLE PRECISION (V)                                     ! E00030
C                                                                         E00040
C**********************************************************************   E00050
C     THIS SUBROUTINE READS IN THE DESIRED "CROSS-SECTION"                E00060
C     MOLECULES WHICH ARE THEN MATCHED TO THE DATA CONTAINED              E00070
C     ON INPUT FILE FSCDXS.                                               E00080
C**********************************************************************   E00090
C                                                                         E00100
C     IFIL CARRIES FILE INFORMATION                                       E00110
C                                                                         E00120
      PARAMETER (MXFSC=200, MXLAY=MXFSC+3,MXZMD=3400,
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=38,MXTRAC=22)
C
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,         E00130
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,        E00140
     *              NLTEFL,LNFIL4,LNGTH4                                  E00150
C                                                                         E00160
C     IXMAX=MAX NUMBER OF X-SECTION MOLECULES, IXMOLS=NUMBER OF THESE     E00170
C     MOLECULES SELECTED, IXINDX=INDEX VALUES OF SELECTED MOLECULES       E00180
C     (E.G. 1=CLONO2), XAMNT(I,L)=LAYER AMOUNTS FOR I'TH MOLECULE FOR     E00190
C     L'TH LAYER, ANALOGOUS TO AMOUNT IN /PATHD/ FOR THE STANDARD         E00200
C     MOLECULES.                                                          E00210
C                                                                         E00220
      COMMON /PATHX/ IXMAX,IXMOLS,IXINDX(MXMOL),XAMNT(MXMOL,MXLAY)        E00230
C                                                                         E00240
C     COMMON BLOCKS AND PARAMETERS FOR THE PROFILES AND DENSITIES         E00250
C     FOR THE CROSS-SECTION MOLECULES.                                    E00260
C     XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES          E00270
C                                                                         E00280
      CHARACTER*10 XSFILE,XSNAME,ALIAS,BLANK                              E00290
      COMMON /XSECTF/ XSFILE(6,5,MXMOL),XSNAME(MXMOL),ALIAS(4,MXMOL)      E00300
      COMMON /XSECTR/ V1FX(5,MXMOL),V2FX(5,MXMOL),DVFX(5,MXMOL),
     *                WXM(MXMOL),NTEMPF(5,MXMOL),NSPECR(MXMOL),
     *                IXFORM(5,MXMOL),
     *                XSMASS(MXMOL),XDOPLR(5,MXMOL),NUMXS,IXSBIN          E00325

      COMMON /CVREXT/    HNAMEXT,HVREXT

      CHARACTER*18       HNAMEXT,HVREXT
C                                                                         E00330
      DIMENSION IXFLG(MXMOL)                                              E00340
C                                                                         E00350
      CHARACTER*120 XSREC                                                 E00360
      CHARACTER*1 CFLG,CASTSK,CPRCNT,CN,CF                                E00370
      EQUIVALENCE (CFLG,XSREC)                                            E00380
C                                                                         E00390
      DATA CASTSK / '*'/,CPRCNT / '%'/,CN / 'N'/,CF / 'F'/                E00400
      DATA BLANK / '          '/                                          E00410
C                                                                         E00411
C     T296 IS TEMPERATURE FOR INITAL CALCULATIN OF DOPPLER WIDTHS         E00412
C                                                                         E00413
      DATA T296 / 296.0 /                                                 E00414
C
      HVREXT = '$Revision: 2.4 $'
C                                                                         E00420
      IXMAX = MXMOL                                                       E00430
      DO 10 I = 1, IXMAX                                                  E00440
         XSNAME(I) = BLANK                                                E00450
   10 CONTINUE                                                            E00460
C                                                                         E00470
C     READ IN THE NAMES OF THE MOLECULES                                  E00480
C                                                                         E00490
      IF (IXMOLS.GT.7) THEN                                               E00500
         READ (IRD,'(7A10)') (XSNAME(I),I=1,7)                            E00510
         READ (IRD,'(8A10)') (XSNAME(I),I=8,IXMOLS)                       E00520
      ELSE                                                                E00530
         READ (IRD,'(7A10)') (XSNAME(I),I=1,IXMOLS)                       E00540
      ENDIF                                                               E00550
C                                                                         E00560
C     Left-justify all inputed names                                      E00570
C                                                                         E00580
      DO 15 I=1,IXMOLS                                                    E00582
         CALL CLJUST (XSNAME(I),10)                                       E00590
 15   CONTINUE
C                                                                         E00600
CPRT  WRITE(IPR,'(/,''  THE FOLLOWING MOLECULES ARE REQUESTED:'',//,      E00610
CPRT 1    (5X,I5,2X,A))') (I,XSNAME(I),I=1,IXMOLS)                        E00620
C                                                                         E00630
C     MATCH THE NAMES READ IN AGAINST THE NAMES STORED IN ALIAS           E00640
C     AND DETERMINE THE INDEX VALUE.  STOP IF NO MATCH IS FOUND.          E00650
C     NAME MUST BE ALL IN CAPS.                                           E00660
C                                                                         E00670
      DO 40 I = 1, IXMOLS                                                 E00680
         DO 20 J = 1, IXMAX                                               E00690
            IF ((XSNAME(I).EQ.ALIAS(1,J)) .OR.                            E00700
     *          (XSNAME(I).EQ.ALIAS(2,J)) .OR.                            E00710
     *          (XSNAME(I).EQ.ALIAS(3,J)) .OR.                            E00720
     *          (XSNAME(I).EQ.ALIAS(4,J))) THEN                           E00730
               IXINDX(I) = J                                              E00740
               GO TO 30                                                   E00750
            ENDIF                                                         E00760
   20    CONTINUE                                                         E00770
C                                                                         E00780
C         NO MATCH FOUND                                                  E00790
C                                                                         E00800
         WRITE (IPR,900) XSNAME(I)                                        E00810
         STOP 'STOPPED IN XSREAD'                                         E00820
C                                                                         E00830
   30    CONTINUE                                                         E00840
         IXFLG(I) = 0                                                     E00850
   40 CONTINUE                                                            E00860
C                                                                         E00870
      RETURN                                                              E01490
C                                                                         E01500
  900 FORMAT (/,'  THE NAME: ',A10, ' IS NOT ONE OF THE ',                E01510
     *        'CROSS SECTION MOLECULES. CHECK THE SPELLING.')             E01520
  905 FORMAT (/)                                                          E01530
  910 FORMAT (A120)                                                       E01540
  915 FORMAT (A10,2F10.4,F10.8,I5,5X,I5,A1,4X,6A10)                       E01550
  920 FORMAT (/,'******* ERROR IN XSREAD ** MOLECULE SECLECTED -',A10,    E01560
     *        '- HAS ',I2,' SPECTRAL REGIONS ON FILE FSCDXS, BUT THE',    E01570
     *        ' MAXIMUM ALLOWED IS 6 *******',/)                          E01580
  925 FORMAT (/,'******* MOLECULE SELECTED -',A10,'- IS NOT FOUND ON',    E01590
     *        ' FILE FSCDXS *******',/)                                   E01600
C                                                                         E01610
      END                                                                 E01620
      BLOCK DATA BXSECT                                                   E01630
C                                                                         E01640
      IMPLICIT DOUBLE PRECISION (V)                                     ! E01650
C                                                                         E01660
C**   XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES          E01670
C**            (NOTE: ALL NAMES ARE LEFT-JUSTIFIED)                       E01680
C                                                                         E01690
      PARAMETER(MXMOL=38)
      CHARACTER*10 XSFILE,XSNAME,ALIAS                                    E01700
      COMMON /XSECTI/ XSMAX(6,5,MXMOL),XSTEMP(6,5,MXMOL),
     *                NPTSFX(5,MXMOL),NFILEX(5,MXMOL),NLIMX  
      COMMON /XSECTF/ XSFILE(6,5,MXMOL),XSNAME(MXMOL),ALIAS(4,MXMOL)
      COMMON /XSECTR/ V1FX(5,MXMOL),V2FX(5,MXMOL),DVFX(5,MXMOL),
     *                WXM(MXMOL),NTEMPF(5,MXMOL),NSPECR(MXMOL),
     *                IXFORM(5,MXMOL),  
     *                XSMASS(MXMOL),XDOPLR(5,MXMOL),NUMXS,IXSBIN    
      COMMON /XSECTS/ JINPUT,NMODES,NPANEL,NDUM,V1XS,V2XS,DVXS,NPTSXS     E02870
C                                                                         E01750
      DATA NMODES / 1 /,NPANEL / 0 /,V1XS / 0.0 /,V2XS / 0.0 /,           E02990
     *     DVXS / 0.0 /,NPTSXS / 0 /                                      E03000
      DATA XSMAX / 1140*0.0 /                                             E03010
      DATA (ALIAS(1,I),I=1,MXMOL)/                                        E01760
     *    'CLONO2    ', 'HNO4      ', 'CHCL2F    ', 'CCL4      ',         E01770
     *    'CCL3F     ', 'CCL2F2    ', 'C2CL2F4   ', 'C2CL3F3   ',         E01780
     *    'N2O5      ', 'HNO3      ', 'CF4       ', 'CHCLF2    ',         E01790
     *    'CCLF3     ', 'C2CLF5    ', 24*' ZZZZZZZZ ' /                   E01800
      DATA (ALIAS(2,I),I=1,MXMOL)/                                        E01810
     *    'CLNO3     ', ' ZZZZZZZZ ', 'CFC21     ', ' ZZZZZZZZ ',         E01820
     *    'CFCL3     ', 'CF2CL2    ', 'C2F4CL2   ', 'C2F3CL3   ',         E01830
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'CHF2CL    ',         E01840
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 24*' ZZZZZZZZ ' /                   E01850
      DATA (ALIAS(3,I),I=1,MXMOL)/                                        E01860
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'CFC21     ', ' ZZZZZZZZ ',         E01870
     *    'CFC11     ', 'CFC12     ', 'CFC114    ', 'CFC113    ',         E01880
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'CFC14     ', 'CFC22     ',         E01890
     *    'CFC13     ', 'CFC115    ', 24*' ZZZZZZZZ ' /                   E01900
      DATA (ALIAS(4,I),I=1,MXMOL)/                                        E01910
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'F21       ', ' ZZZZZZZZ ',         E01920
     *    'F11       ', 'F12       ', 'F114      ', 'F113      ',         E01930
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'F14       ', 'F22       ',         E01940
     *    'F13       ', 'F115      ', 24*' ZZZZZZZZ ' /                   E01950
C                                                                         E01960
C     XSMASS IS MASS OF EACH CROSS-SECTION                                E01961
C                                                                         E01962
      DATA XSMASS/                                                        E01963
     1      97.46     ,   79.01     ,  102.92     ,  153.82     ,         E01964
     2     137.37     ,  120.91     ,  170.92     ,  187.38     ,         E01965
     3     108.01     ,   63.01     ,   88.00     ,   86.47     ,         E01966
     4     104.46     ,  154.47     ,  24*0.00 /                          E01967
C                                                                         E01969
      DATA V1FX / 190*0.0 /,V2FX / 190*0.0 /,DVFX / 190*0.0 /,            E01970
     *     WXM / 38*0.0 /                                                 E01980
      DATA NTEMPF / 190*0 /,NSPECR / 38*0 /,IXFORM / 190*0 /,             E01990
     *     NUMXS / 0 /                                                    E02000
C                                                                         E02010
      END                                                                 E02020
      SUBROUTINE CLJUST (CNAME,NCHAR)                                     E02030
C                                                                         E02040
C     THIS SUBROUTINE LEFT-JUSTIFIES THE CHARACTER CNAME                  E02050
C                                                                         E02060
      CHARACTER*(*) CNAME                                                 E02070
      CHARACTER*25 CTEMP                                                  E02070
      CHARACTER*1  CTEMP1(25),BLANK                                       E02080
      EQUIVALENCE (CTEMP,CTEMP1(1))                                       E02090

      COMMON /CVREXT/    HNAMEXT,HVREXT

      CHARACTER*18       HNAMEXT,HVREXT

C                                                                         E02100
      DATA BLANK / ' '/                                                   E02110
C                                                                         E02120
         CTEMP = CNAME                                                    E02140
         JJ=0                                                             E02145
         DO 10 J = 1, NCHAR                                               E02150
            IF (CTEMP1(J).NE.BLANK) THEN                                  E02160
               JJ = J                                                     E02170
               IF (JJ.EQ.1) GO TO 50                                      E02180
               GO TO 20                                                   E02190
            ENDIF                                                         E02200
   10    CONTINUE                                                         E02210
         IF (JJ .EQ. 0) GO TO 50                                          E02215
C                                                                         E02220
   20    KCNT = 0                                                         E02230
         DO 30 K = JJ, NCHAR                                              E02240
            KCNT = KCNT+1                                                 E02250
            CTEMP1(KCNT) = CTEMP1(K)                                      E02260
   30    CONTINUE                                                         E02270
C                                                                         E02280
         KK = NCHAR-JJ+2                                                  E02290
         DO 40 L = KK,NCHAR                                               E02300
            CTEMP1(L) = BLANK                                             E02310
   40    CONTINUE                                                         E02320
         CNAME = CTEMP                                                    E02330
   50 CONTINUE                                                            E02340
C                                                                         E02350
      RETURN                                                              E02360
C                                                                         E02370
      END                                                                 E02380
      SUBROUTINE EXPINT (X,X1,X2,A)                                       A10600
C                                                                         A10610
C**********************************************************************   A10620
C     THIS SUBROUTINE EXPONENTIALLY INTERPOLATES X1 AND X2 TO X BY        A10630
C     THE FACTOR A                                                        A10640
C**********************************************************************   A10650
C                                                                         A10660

      COMMON /CVREXT/    HNAMEXT,HVREXT

      CHARACTER*18       HNAMEXT,HVREXT

      IF (X1.EQ.0.0.OR.X2.EQ.0.0) GO TO 10                                A10670
      X = X1*(X2/X1)**A                                                   A10680
C                                                                         A10690
      RETURN                                                              A10700
C                                                                         A10710
   10 X = X1+(X2-X1)*A                                                    A10720
C                                                                         A10730
      RETURN                                                              A10740
C                                                                         A10750
      END                                                                 A10760
