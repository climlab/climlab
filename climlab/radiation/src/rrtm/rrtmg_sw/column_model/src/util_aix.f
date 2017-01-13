C     path:      %P%
C     revision:  $Revision: 7.02 $
C     created:   $Date: 2003/02/10 21:21:16 $  
C     presently: %H%  %T%
                                           
c==============================================================================
c
c NAME:
c       lbldat
c
c CALLING SEQUENCE:
c       CALL lbldat( hdate )
c
c PURPOSE:
c       Returns the date, in MM/DD/YY format, in a double-precision 
c       variable (also 8-bytes).
c
c PROCEDURE:
c       Use AIX XL Fortran Service and Utility Procedure, DATE, to return 
c       the current date in MM/DD/YY format. This value is then read into
c       the double precision variable, hdate, using an internal read
c       statement.
c
c==============================================================================

      SUBROUTINE lbldat( hdate )


c     ---------
c     Arguments
c     ---------

      REAL*8 hdate


c     ---------------
c     Local variables
c     ---------------

      CHARACTER*8 d

c     ---------
c     Functions
c     ---------

      CHARACTER*8 DATE

      COMMON /CVRUTL/ HNAMUTL,HVRUTL
C
      CHARACTER*18 HNAMUTL,HVRUTL
C

c#######################################################################
c                     ## BEGIN EXECUTABLE CODE ##
c#######################################################################

C
C     ASSIGN NAME and CVS VERSION NUMBER TO MODULE 
C
      HNAMUTL= '       util_aix.f:'
      HVRUTL = '$Revision: 7.02 $' 




c     --------------------
c     Get the current date
c     --------------------

      d = DATE()


c     ---------------------------------------
c     Read the character date into the double
c     precision variable argument.
c     ---------------------------------------

      READ( d, '( a8 )' ) hdate


c     ----
c     Done
c     ----

      RETURN
      END





c==============================================================================
c
c NAME:
c       ftime
c
c PURPOSE:
c       Returns the time, in HH:MM:SS format, in a double-precision 
c       variable (also 8-bytes).
c
c CALLING SEQUENCE:
c       CALL ftime( htime )
c
c PROCEDURE:
c       Use AIX XL Fortran Service and Utility Procedure, CLOCK_, to return 
c       the current time in HH:MM:SS format. This value is then read into
c       the double precision variable, htime, using an internal read
c       statement.
c
c==============================================================================

      SUBROUTINE ftime( htime )


c     ---------
c     Arguments
c     ---------

      REAL*8 htime


c     ---------------
c     Local variables
c     ---------------

      CHARACTER*8 c


c     ---------
c     Functions
c     ---------

      CHARACTER*8 CLOCK_



c#######################################################################
c                     ## BEGIN EXECUTABLE CODE ##
c#######################################################################

c     --------------------
c     Get the current time
c     --------------------

      c = CLOCK_()


c     ---------------------------------------
c     Read the character time into the double
c     precision variable argument.
c     ---------------------------------------

      READ( c, '( a8 )' ) htime


c     ----
c     Done
c     ----

      RETURN
      END
      SUBROUTINE CPUTIM (TIME)                                           LN05770
C                                                                        LN05780
      COMMON /TIMIN/ A1                                                  LN05790
      INTEGER*4 CPU_TIME
C                                                                        LN05800
C     REAL*4 ETIME,TARRAY(2)                                            >LN05810
C                                                                        LN05820
C     THIS SUBROUTINE OBTAINS CPU TIME                                   LN05830
C                                                                        LN05840
      IF (A1.LE.0.) THEN                                                 LN05850
C>>      CALL SECOND (TIME)                                              LN05860
C>VAX    A1 = SECNDS(0.0)                                               >LN05870
C        TIME = ETIME(TARRAY)                                           >LN05880
         CPU_TIME = MCLOCK()
         TIME =  REAL(CPU_TIME)/100
      ELSE                                                               LN05890
C>>      CALL SECOND (TIME)                                              LN05900
C>VAX    TIME = SECNDS(A1)                                              >LN05910
C        TIME = ETIME(TARRAY)                                           >LN05920
         CPU_TIME = MCLOCK()
         TIME =  REAL(CPU_TIME)/100
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
      COMMON /CVRUTL/ HVRUTL
C
      CHARACTER*15 HVRUTL
C
      DATA i_4 / 4 / 
C
      DIMENSION IARRAY(IWORDS)
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

      SUBROUTINE BUFIN_sgl(IFILE,IEOF,IARRAY,IWORDS)
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

      SUBROUTINE BUFOUT_sgl(IFILE,IARRAY,IWORDS)
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

