!     path:      %P%                                                    
!     revision:  $Revision: 10548 $                                     
!     created:   $Date: 2011-10-07 13:05:24 -0400 (Fri, 07 Oct 2011) $  
!     presently: %H%  %T%                                               
!     --------------------------------------------------------------    
      SUBROUTINE LBLDAT(HDATE) 
!                                                                       
      EXTERNAL IDATE 
!                                                                       
      character*8 hdate 
!                                                                       
      CHARACTER GDATE*10 
!                                                                       
      INTEGER*4 IARRAY(3) 
!                                                                       
      COMMON /CVRUTL/ HNAMUTL,HVRUTL 
!                                                                       
      CHARACTER*18 HNAMUTL,HVRUTL 
!                                                                       
!     ASSIGN NAME and CVS VERSION NUMBER TO MODULE                      
!                                                                       
      HNAMUTL= '     util_alpha.f:' 
      HVRUTL = '$Revision: 10548 $' 
                                                                        
      CALL IDATE(IARRAY) 
                                                                        
      IARRAY(3)=iarray(3)-(int(iarray(3)/100))*100 
!                                                                       
      WRITE (GDATE,900) IARRAY(3),IARRAY(2),IARRAY(1) 
!                                                                       
      READ (GDATE,901) HDATE 
!                                                                       
      RETURN 
!                                                                       
  900 FORMAT (   I2.2,2('/',I2.2)) 
  901 FORMAT (A8) 
!                                                                       
      END                                           
!                                                                       
!     --------------------------------------------------------------    
      SUBROUTINE FTIME (HTIME) 
!                                                                       
      CHARACTER*8 htime 
!                                                                       
      CHARACTER GTIME*10 
!                                                                       
      INTEGER*4 IARRAY(5) 
!                                                                       
      CALL ITIME (IARRAY) 
      WRITE (GTIME,900) IARRAY(1),IARRAY(3),IARRAY(5) 
!                                                                       
      READ (GTIME,901) HTIME 
!                                                                       
      RETURN 
!                                                                       
  900 FORMAT (I2,2(':',I2.2)) 
  901 FORMAT (A8) 
!                                                                       
      END                                           
!                                                                       
!     --------------------------------------------------------------    
      SUBROUTINE CPUTIM (TIME) 
!                                                                       
      COMMON /TIMIN/ A1 
!                                                                       
      REAL*4 ETIME,TARRAY(2) 
!                                                                       
!     THIS SUBROUTINE OBTAINS CPU TIME                                  
!                                                                       
      IF (A1.LE.0.) THEN 
         TIME = ETIME(TARRAY) 
      ELSE 
         TIME = ETIME(TARRAY) 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
      END                                           
!                                                                       
!     --------------------------------------------------------------    
      BLOCK DATA BTIM 
!                                                                       
      COMMON /TIMIN/ A1 
!                                                                       
      DATA A1 / 0. / 
!                                                                       
      END                                           
                                                                        
                                                                        
!     --------------------------------------------------------------    
      SUBROUTINE BUFIN (IFILE,IEOF,IARRAY,IWORDS) 
!                                                                       
!     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING   
!     AT LOCATION IARRAY                                                
!                                                                       
!     IFILE IS THE FILE DESIGNATION                                     
!                                                                       
      DATA i_4 / 4 / 
!                                                                       
      DIMENSION IARRAY(IWORDS) 
                                                                        
      IEOF = 1 
!                                                                       
      READ (IFILE,END=10) IARRAY 
      ITEST = MIN(IWORDS,i_4) 
      IF (IARRAY(ITEST).EQ.-99) IEOF = -99 
!                                                                       
      RETURN 
!                                                                       
   10 IEOF = 0 
!                                                                       
      RETURN 
!                                                                       
      END                                           
!                                                                       
!     --------------------------------------------------------------    
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>                         
                                                                        
!         note the name change                                          
                                                                        
      SUBROUTINE BUFIN_sgl(IFILE,IEOF,IARRAY,IWORDS) 
!                                                                       
!     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING   
!     AT LOCATION IARRAY                                                
!                                                                       
!     IFILE IS THE FILE DESIGNATION                                     
!                                                                       
                                                                        
      implicit integer*4 (i-n) 
      implicit real*4    (a-h,o-z) 
                                                                        
      DATA i_4 / 4 / 
                                                                        
      DIMENSION IARRAY(IWORDS) 
!                                                                       
      IEOF = 1 
!                                                                       
!#    BUFFER IN (IFILE,1) (IARRAY(ILO),IARRAY(IHI))                     
!#    IF (UNIT(IFILE).EQ.0.) GO TO 10                                   
!                                                                       
      READ (IFILE,END=10) IARRAY 
      ITEST = MIN(IWORDS,i_4) 
      IF (IARRAY(ITEST).EQ.-99) IEOF = -99 
!                                                                       
      RETURN 
!                                                                       
   10 IEOF = 0 
!                                                                       
      RETURN 
!                                                                       
      END                                           
!_______________________________________________________________________
                                                                        
      SUBROUTINE BUFOUT (IFILE,IARRAY,IWORDS) 
!                                                                       
!     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING  
!     AT LOCATION IARRAY                                                
!                                                                       
!     IFILE IS THE FILE DESIGNATION                                     
!                                                                       
      DIMENSION IARRAY(IWORDS) 
!                                                                       
      WRITE (IFILE) IARRAY 
!                                                                       
      RETURN 
!                                                                       
      END                                           
!                                                                       
!_______________________________________________________________________
                                                                        
      SUBROUTINE BUFOUT_sgl(IFILE,IARRAY,IWORDS) 
!                                                                       
!     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING  
!     AT LOCATION IARRAY                                                
!                                                                       
!     IFILE IS THE FILE DESIGNATION                                     
!                                                                       
!                                                                       
      implicit integer*4 (i-n) 
      implicit real*4    (a-h,o-z) 
!                                                                       
      DIMENSION IARRAY(IWORDS) 
!                                                                       
!#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))                    
!#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '                   
!                                                                       
      WRITE (IFILE) IARRAY 
!                                                                       
      RETURN 
!                                                                       
      END                                           
