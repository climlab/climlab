C     path:      %P%
C     revision:  $Revision: 7.1 $
C     created:   $Date: 2006/06/08 13:20:22 $  
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
c       Returns the date, in MM/DD/YY format, in a 8 byte variable.
c
c PROCEDURE:
c       Use IBM XL Fortran Service and Utility Procedure, DATE, to return 
c       the current date in MM/DD/YY format. This value is then read into
c       the double precision variable, hdate, using an internal read
c       statement.
c
c==============================================================================

      SUBROUTINE lbldat( hdate )

c     ---------
c     Arguments
c     ---------

      character*8 hdate

c     ---------
c     Common
c     ---------

      COMMON /CVRUTL/ HNAMUTL,HVRUTL
C
      CHARACTER*18 HNAMUTL,HVRUTL
C
c     ---------------
c     Local variables
c     ---------------

      character*2 hyear,hmonth,hday

c     ---------
c     Functions
c     ---------

      character(8)  :: date
      character(10) :: time
      character(5)  :: zone
      integer,dimension(8) :: ivals


c#######################################################################
c                     ## BEGIN EXECUTABLE CODE ##
c#######################################################################

C
C     ASSIGN NAME and CVS VERSION NUMBER TO MODULE 
C
      HNAMUTL= '       util_g95.f:'
      HVRUTL = '$Revision: 7.1 $' 

c     --------------------
c     Get the current date
c     --------------------

      call date_and_time(date,time,zone,ivals)

c     ---------------------------------------
c     Read the character date into the double
c     precision variable argument.
c     ---------------------------------------

      READ  ( date, 900 ) hyear,hmonth,hday

      write ( hdate,905 ) hyear,hmonth,hday

 900  format(2x,a2,a2,a2)
 905  format(a2,'/',a2,'/',a2)

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
c       Returns the time, in HH:MM:SS format, in an 8 byte character variable
c
c CALLING SEQUENCE:
c       CALL ftime( htime )
c
c PROCEDURE:
c       Use XLF Fortran Service and Utility Procedure, CLOCK_, to return 
c       the current time in HH:MM:SS format. This value is then read into
c       the double precision variable, htime, using an internal read
c       statement.
c
c==============================================================================

      SUBROUTINE ftime( htime )

c     ---------
c     Arguments
c     ---------

      CHARACTER*8 htime


c     ---------------
c     Local variables
c     ---------------

      character*2 hhrs,hmins,hsec

c     ---------
c     Functions
c     ---------

      character(8)  :: date
      character(10) :: time
      character(5)  :: zone
      integer,dimension(8) :: ivals

c#######################################################################
c                     ## BEGIN EXECUTABLE CODE ##
c#######################################################################

c     --------------------
c     Get the current time
c     --------------------

      call date_and_time(date,time,zone,ivals)

c     ---------------------------------------
c     Read the character time into the double
c     precision variable argument.
c     ---------------------------------------

      READ  ( time, 900 ) hhrs,hmins,hsec

      write ( htime,905 ) hhrs,hmins,hsec

 900  format(a2,a2,a2)
 905  format(a2,':',a2,':',a2)

c     ----
c     Done
c     ----

      RETURN
      END

c==============================================================================


      SUBROUTINE CPUTIM (TIME)                                           LN05770
C                                                                        LN05780
      REAL*4 time_r4
C                                                                        LN05820

c     The CPU_TIME xlf system subroutine returns the CPU time, in seconds, 
c     taken by the current process and, possibly, all the child processes 
c     in all of the threads. A call to CPU_TIME will give the processor time 
c     taken by the process from the start of the program. The time measured 
c     only accounts for the amount of time that the program is actually running, 
c     and not the time that a program is suspended or waiting.

      call cpu_time(time_r4)
      time = time_r4

C                                                                        LN05940
      RETURN                                                             LN05950
C                                                                        LN05960
      END                                                                LN05970


c==============================================================================

      SUBROUTINE BUFIN (IFILE,IEOF,IARRAY,IWORDS)
C
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING
C     AT LOCATION IARRAY
C
C     IFILE IS THE FILE DESIGNATION
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

