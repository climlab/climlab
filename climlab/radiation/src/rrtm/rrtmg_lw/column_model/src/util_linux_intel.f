C     path:      %P%
C     revision:  $Revision: 7.2 $
C     created:   $Date: 2004/05/27 17:46:59 $  
C     presently: %H%  %T%
c______________________________________________________________________________

      SUBROUTINE LBLDAT(HDATE)                                           LN05190
C                                                                        LN05200
      character*8 hdate
      integer*4 iday,imon,iyear
C                                                                        LN05220
      CHARACTER GDATE*8

      COMMON /CVRUTL/ HNAMUTL,HVRUTL
C
      CHARACTER*18 HNAMUTL,HVRUTL
C
C     ASSIGN NAME and CVS VERSION NUMBER TO MODULE 
C
      HNAMUTL= 'util_linux_intel.f:'
      HVRUTL = '$Revision: 7.2 $' 

C                                                                        LN05240
      CALL IDATE(iday,imon,iyear) 
c
      iyear=iyear-(100*(iyear/100))
c
      WRITE (GDATE,900) iyear,imon,iday
C                                                                        LN05310
      READ (GDATE,901) HDATE                                             LN05320
C                                                                        LN05330
C     CHANGE THESE TO WORD SIZE AND FORMAT OF ROUTINE DATE               LN05390
C                                                                        LN05400
      RETURN                                                             LN05410
C                                                                        LN05420
  900 FORMAT (   I2.2,2('/',I2.2))                                         LN05430
  901 FORMAT (A8)                                                        LN05450
C                                                                        LN05460
      END                                                                LN05470
      SUBROUTINE FTIME (HTIME)                                           LN05480
C                                                                        LN05490
C                                                                        LN05510
      CHARACTER*8 gtime,htime
C                                                                        LN05530
      INTEGER*4 IARRAY(3)                                               >LN05540
C                                                                        LN05550
      CALL ITIME (IARRAY)                                               >LN05580
c
      WRITE (GTIME,900) IARRAY                                          >LN05590
C                                                                        LN05600
      READ (GTIME,901) HTIME                                             LN05610
C                                                                        LN05620
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
         A1 =  ETIME(TARRAY)                                            >LN05880
         TIME = a1
      ELSE                                                               LN05890
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
C                                  
      DATA i_4 / 4 /    
c
      DIMENSION IARRAY(IWORDS)
C                                                                         A10830
C                          
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
