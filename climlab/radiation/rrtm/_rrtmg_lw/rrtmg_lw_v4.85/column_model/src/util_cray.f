C     path:      %P%
C     revision:  $Revision: 7.02 $
C     created:   $Date: 2003/02/10 21:21:17 $  
C     presently: %H%  %T%
c______________________________________________________________________________

      SUBROUTINE LBLDAT(HDATE)                                            A07730
C                                                                         A07740
      character*8 hdate
C                                                                         A07760
      CHARACTER GDATE*10                                                  A07770
C                                                                         A07800
      COMMON /CVRUTL/ HNAMUTL,HVRUTL
C
      CHARACTER*18 HNAMUTL,HVRUTL
C
C
C     ASSIGN NAME and CVS VERSION NUMBER TO MODULE 
C
      HNAMUTL= '       util_sgi.f:'
      HVRUTL = '$Revision: 7.02 $' 
C
      CALL DATE (GDATE)                                                 > A07810
C                                                                         A07850
      READ (GDATE,901) HDATE                                              A07860
C                                                                         A07870
C     GDATE AND FORMAT ARE FOR CYBER AND CRAY                             A07880
C                                                                         A07890
C       -- CYBER REQUIRES FORMAT (1X,A8)                                  A07900
C       -- CRAY  REQUIRES FORMAT (A8)                                     A07910
C                                                                         A07920
C     CHANGE THESE TO WORD SIZE AND FORMAT OF ROUTINE DATE                A07930
C                                                                         A07940
      RETURN                                                              A07950
C                                                                         A07960
  900 FORMAT (1X,I2,2('/',I2.2))                                          A07970
C>901 FORMAT (1X,A8)                                                    > A07980
  901 FORMAT (A8)                                                       > A07990
C                                                                         A08000
      END                                                                 A08010
      SUBROUTINE FTIME (HTIME)                                            A08020
C                                                                         A08050
      CHARACTER GTIME*10                                                  A08060
C                                                                         A08070
      CALL CLOCK (GTIME)                                                > A08100
C                                                                         A08140
      READ (GTIME,901) HTIME                                              A08150
C                                                                         A08160
C     GTIME AND FORMAT ARE FOR CYBER AND CRAY                             A08170
C                                                                         A08180
C       -- CYBER REQUIRES FORMAT (1X,A8)                                  A08190
C       -- CRAY  REQUIRES FORMAT (A8)                                     A08200
C                                                                         A08210
C     CHANGE THESE TO WORD SIZE AND FORMAT OF ROUTINE GTIME               A08220
C                                                                         A08230
      RETURN                                                              A08240
C                                                                         A08250
  900 FORMAT (1X,I2,2(':',I2.2))                                          A08260
C>901 FORMAT (1X,A8)                                                    > A08270
  901 FORMAT (A8)                                                       > A08280
C                                                                         A08290
      END                                                                 A08300
      SUBROUTINE CPUTIM (TIME)                                            A08310
C                                                                         A08320
      COMMON /TIMIN/ A1                                                   A08330
C                                                                         A08340
C>UNX REAL*4 ETIME,TARRAY(2)                                            > A08350
C                                                                         A08360
C     THIS SUBROUTINE OBTAINS CPU TIME                                    A08370
C                                                                         A08380
      IF (A1.LE.0.) THEN                                                  A08390
         CALL SECOND (TIME)                                             > A08400
C>VAX    A1 = SECNDS(0.0)                                               > A08410
C>UNX    TIME = ETIME(TARRAY)                                           > A08420
      ELSE                                                                A08430
         CALL SECOND (TIME)                                             > A08440
C>VAX    TIME = SECNDS(A1)                                              > A08450
C>UNX    TIME = ETIME(TARRAY)                                           > A08460
      ENDIF                                                               A08470
C                                                                         A08480
      RETURN                                                              A08490
C                                                                         A08500
      END                                                                 A08510
      BLOCK DATA BTIM                                                     A08520
C                                                                         A08530
      COMMON /TIMIN/ A1                                                   A08540
C                                                                         A08550
      DATA A1 / 0. /                                                      A08560
C                                                                         A08570
      END                                                                 A08580

      SUBROUTINE BUFIN (IFILE,IEOF,IARRAY,IWORDS)                         A10770
C                                                                         A10780
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING     A10790
C     AT LOCATION IARRAY                                                  A10800
C                                                                         A10810
C     IFILE IS THE FILE DESIGNATION                                       A10820
c
      DATA i_4 / 4 /    
C                                                                         A10850
      DIMENSION IARRAY(IWORDS)                                            A10840

      IEOF = 1                                                            A10860
C                                                                         A10880
C#    BUFFER IN (IFILE,1) (IARRAY(ILO),IARRAY(IHI))                       A10890
C#    IF (UNIT(IFILE).EQ.0.) GO TO 10                                     A10900
C                                                                         A10910
      READ (IFILE,END=10) IARRAY                                          A10920
      ITEST = MIN(IWORDS,i_4)                                               A10930
      IF (IARRAY(ITEST).EQ.-99) IEOF = -99                                A10940
C                                                                         A10950
      RETURN                                                              A10960
C                                                                         A10970
   10 IEOF = 0                                                            A10980
C                                                                         A10990
      RETURN                                                              A11000
C                                                                         A11010
      END                                                                 A11020
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c         note the name change

      SUBROUTINE BUFIN_sgl (IFILE,IEOF,IARRAY,IWORDS)
C
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING
C     AT LOCATION IARRAY
C
C     IFILE IS THE FILE DESIGNATION
C                                  
      implicit integer*4 (i-n)
      implicit real*4    (a-h,o-z)

      DATA i_4 / 4 /    

      DIMENSION IARRAY(IWORDS)
C                                                                         A10830
      IEOF = 1             
C                          
C#    BUFFER IN (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) GO TO 10              
C                                               
      READ (IFILE,END=10) IARRAY
      ITEST = MIN(IWORDS,i_4)                 
      IF (IARRAY(ITEST).EQ.-99) IEOF = -99      
C                                               
      RETURN                                    
C                                               
   10 IEOF = 0                                  
C                                               
      RETURN                                    
C                                               
      END                                       
c______________________________________________________________________________

      SUBROUTINE BUFOUT (IFILE,IARRAY,IWORDS)                             A11030
C                                                                         A11040
C     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING    A11050
C     AT LOCATION IARRAY                                                  A11060
C                                                                         A11070
C     IFILE IS THE FILE DESIGNATION                                       A11080
C                                                                         A11090
      DIMENSION IARRAY(IWORDS)                                            A11100
C                                                                         A11120
C#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))                      A11130
C#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '                     A11140
C                                                                         A11150
      WRITE (IFILE) IARRAY                                                A11160
C                                                                         A11170
      RETURN                                                              A11180
C                                                                         A11190
      END                                                                 A11200
c______________________________________________________________________________

      SUBROUTINE BUFOUT_sgl (IFILE,IARRAY,IWORDS)
C                                                 
C     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING
C     AT LOCATION IARRAY                                                 
C                                                                     
C     IFILE IS THE FILE DESIGNATION                                   
C                                                                     
c
      implicit integer*4 (i-n)
      implicit real*4    (a-h,o-z)
c
      DIMENSION IARRAY(IWORDS)
C                                                   
C#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '
C                                                    
      WRITE (IFILE) IARRAY
C                                                    
      RETURN                                         
C                                                    
      END                                            
