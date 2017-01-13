C     path:      %P%
C     revision:  $Revision: 7.02 $
C     created:   $Date: 2003/02/10 21:21:17 $  
C     presently: %H%  %T%
c______________________________________________________________________________

      SUBROUTINE LBLDAT(HDATE)                                           LN05190
C                                                                        LN05200
      character*8 hdate
C                                                                        LN05220
      CHARACTER GDATE*10                                                 LN05230
C                                                                        LN05240
      INTEGER*4 IARRAY(3)                                               >LN05250

      COMMON /CVRUTL/ HNAMUTL,HVRUTL
C
      CHARACTER*18HNAMUTL,HVRUTL
C                                  
C
C     ASSIGN NAME and CVS VERSION NUMBER TO MODULE 
C
      HNAMUTL= '        util_hp.f:'
      HVRUTL = '$Revision: 7.02 $' 

C                                                                        LN05260
C>>   CALL DATE (GDATE)                                                  LN05270

      CALL IDATE(IARRAY)                                                >LN05280
      IARRAY(3)=iarray(3)-(int(iarray(3)/100))*100
      WRITE (GDATE,900) IARRAY(3),IARRAY(2),IARRAY(1)                   >LN05300
C                                                                        LN05310
      READ (GDATE,901) HDATE                                             LN05320
C                                                                        LN05330
C     GDATE AND FORMAT ARE FOR CYBER AND CRAY                            LN05340
C                                                                        LN05350
C       -- CYBER REQUIRES FORMAT (1X,A8)                                 LN05360
C       -- CRAY  REQUIRES FORMAT (A8)                                    LN05370
C                                                                        LN05380
C     CHANGE THESE TO WORD SIZE AND FORMAT OF ROUTINE DATE               LN05390
C                                                                        LN05400
      RETURN                                                             LN05410
C                                                                        LN05420
C>900 FORMAT (1X,I2,2('/',I2.2))                                         LN05430
c  900 FORMAT (   I2,2('/',I2.2))                                         LN05430
  900 FORMAT (   I2.2,2('/',I2.2))
C>901 FORMAT (1X,A8)                                                    >LN05440
  901 FORMAT (A8)                                                        LN05450
C                                                                        LN05460
      END                                                                LN05470
      SUBROUTINE FTIME (HTIME)                                           LN05480
C                                                                        LN05490
      DOUBLE PRECISION HTIME                                            &LN05500
C                                                                        LN05510
      CHARACTER GTIME*10                                                 LN05520
C                                                                        LN05530
      INTEGER*4 IARRAY(3)                                               >LN05540
C                                                                        LN05550
C>>   CALL CLOCK (GTIME)                                                 LN05560
C>VAX CALL TIME (GTIME)                                                 >LN05570
      CALL ITIME (IARRAY)                                               >LN05580
      WRITE (GTIME,900) IARRAY                                          >LN05590
C                                                                        LN05600
      READ (GTIME,901) HTIME                                             LN05610
C                                                                        LN05620
C     GTIME AND FORMAT ARE FOR CYBER AND CRAY                            LN05630
C                                                                        LN05640
C       -- CYBER REQUIRES FORMAT (1X,A8)                                 LN05650
C       -- CRAY  REQUIRES FORMAT (A8)                                    LN05660
C                                                                        LN05670
C     CHANGE THESE TO WORD SIZE AND FORMAT OF ROUTINE GTIME              LN05680
C                                                                        LN05690
      RETURN                                                             LN05700
C                                                                        LN05710
C>900 FORMAT (1X,I2,2(':',I2.2))                                         LN05720
  900 FORMAT (   I2,2(':',I2.2))                                         LN05720
C>901 FORMAT (1X,A8)                                                    >LN05730
  901 FORMAT (A8)                                                        LN05740
C                                                                        LN05750
      END                                                                LN05760
      SUBROUTINE CPUTIM (TIME)                                           LN05770
C                                                                        LN05780
      COMMON /TIMIN/ A1                                                  LN05790
C                                                                        LN05800
      REAL*4 ETIME,TARRAY(2)                                            >LN05810
C                                                                        LN05820
C     THIS SUBROUTINE OBTAINS CPU TIME                                   LN05830
C                                                                        LN05840
      IF (A1.LE.0.) THEN                                                 LN05850
C>>      CALL SECOND (TIME)                                              LN05860
C>VAX    A1 = SECNDS(0.0)                                               >LN05870
         TIME = ETIME(TARRAY)                                           >LN05880
      ELSE                                                               LN05890
C>>      CALL SECOND (TIME)                                              LN05900
C>VAX    TIME = SECNDS(A1)                                              >LN05910
         TIME = ETIME(TARRAY)                                           >LN05920
      ENDIF                                                              LN05930
C                                                                        LN05940
      RETURN                                                             LN05950
C                                                                        LN05960
      END                                                                LN05970
      BLOCK DATA BTIM                                                    LN05980
C                                                                        LN05990
      COMMON /TIMIN/ A1                                                  LN06000
C                                                                        LN06010
      DATA A1 / 0. /                                                     LN06020
C                                                                        LN06030
      END                                                                LN06040

      SUBROUTINE BUFIN (IFILE,IEOF,IARRAY,IWORDS)
C
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING
C     AT LOCATION IARRAY
C
C     IFILE IS THE FILE DESIGNATION
C                                                                         A10830
      DATA i_4 / 4 /    
C                          
      DIMENSION IARRAY(IWORDS)

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

      SUBROUTINE BUFOUT (IFILE,IARRAY,IWORDS)
C                                                 
C     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING
C     AT LOCATION IARRAY                                                 
C                                                                     
C     IFILE IS THE FILE DESIGNATION                                   
C                                                                     
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
