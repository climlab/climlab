!     path:      %P%                                                    
!     revision:  $Revision: 10548 $                                     
!     created:   $Date: 2011-10-07 13:05:24 -0400 (Fri, 07 Oct 2011) $  
!     presently: %H%  %T%                                               
                                                                        
!=======================================================================
!                                                                       
! NAME:                                                                 
!       lbldat                                                          
!                                                                       
! CALLING SEQUENCE:                                                     
!       CALL lbldat( hdate )                                            
!                                                                       
! PURPOSE:                                                              
!       Returns the date, in MM/DD/YY format, in a 8 byte variable.     
!                                                                       
! PROCEDURE:                                                            
!       Use IBM XL Fortran Service and Utility Procedure, DATE, to retur
!       the current date in MM/DD/YY format. This value is then read int
!       the double precision variable, hdate, using an internal read    
!       statement.                                                      
!                                                                       
!=======================================================================
                                                                        
      SUBROUTINE lbldat( hdate ) 
                                                                        
!     ---------                                                         
!     Arguments                                                         
!     ---------                                                         
                                                                        
      character*8 hdate 
                                                                        
!     ---------                                                         
!     Common                                                            
!     ---------                                                         
                                                                        
      COMMON /CVRUTL/ HNAMUTL,HVRUTL 
!                                                                       
      CHARACTER*18 HNAMUTL,HVRUTL 
!                                                                       
!     ---------------                                                   
!     Local variables                                                   
!     ---------------                                                   
                                                                        
      character*2 hyear,hmonth,hday 
                                                                        
!     ---------                                                         
!     Functions                                                         
!     ---------                                                         
                                                                        
      character(8)  :: date 
      character(10) :: time 
      character(5)  :: zone 
      integer,dimension(8) :: ivals 
                                                                        
                                                                        
!#######################################################################
!                     ## BEGIN EXECUTABLE CODE ##                       
!#######################################################################
                                                                        
!                                                                       
!     ASSIGN NAME and CVS VERSION NUMBER TO MODULE                      
!                                                                       
      HNAMUTL= '       util_g95.f:' 
      HVRUTL = '$Revision: 10548 $' 
                                                                        
!     --------------------                                              
!     Get the current date                                              
!     --------------------                                              
                                                                        
      call date_and_time(date,time,zone,ivals) 
                                                                        
!     ---------------------------------------                           
!     Read the character date into the double                           
!     precision variable argument.                                      
!     ---------------------------------------                           
                                                                        
      READ  ( date, 900 ) hyear,hmonth,hday 
                                                                        
      write ( hdate,905 ) hyear,hmonth,hday 
                                                                        
  900 format(2x,a2,a2,a2) 
  905 format(a2,'/',a2,'/',a2) 
                                                                        
!     ----                                                              
!     Done                                                              
!     ----                                                              
                                                                        
      RETURN 
      END                                           
                                                                        
!=======================================================================
!                                                                       
! NAME:                                                                 
!       ftime                                                           
!                                                                       
! PURPOSE:                                                              
!       Returns the time, in HH:MM:SS format, in an 8 byte character var
!                                                                       
! CALLING SEQUENCE:                                                     
!       CALL ftime( htime )                                             
!                                                                       
! PROCEDURE:                                                            
!       Use XLF Fortran Service and Utility Procedure, CLOCK_, to return
!       the current time in HH:MM:SS format. This value is then read int
!       the double precision variable, htime, using an internal read    
!       statement.                                                      
!                                                                       
!=======================================================================
                                                                        
      SUBROUTINE ftime( htime ) 
                                                                        
!     ---------                                                         
!     Arguments                                                         
!     ---------                                                         
                                                                        
      CHARACTER*8 htime 
                                                                        
                                                                        
!     ---------------                                                   
!     Local variables                                                   
!     ---------------                                                   
                                                                        
      character*2 hhrs,hmins,hsec 
                                                                        
!     ---------                                                         
!     Functions                                                         
!     ---------                                                         
                                                                        
      character(8)  :: date 
      character(10) :: time 
      character(5)  :: zone 
      integer,dimension(8) :: ivals 
                                                                        
!#######################################################################
!                     ## BEGIN EXECUTABLE CODE ##                       
!#######################################################################
                                                                        
!     --------------------                                              
!     Get the current time                                              
!     --------------------                                              
                                                                        
      call date_and_time(date,time,zone,ivals) 
                                                                        
!     ---------------------------------------                           
!     Read the character time into the double                           
!     precision variable argument.                                      
!     ---------------------------------------                           
                                                                        
      READ  ( time, 900 ) hhrs,hmins,hsec 
                                                                        
      write ( htime,905 ) hhrs,hmins,hsec 
                                                                        
  900 format(a2,a2,a2) 
  905 format(a2,':',a2,':',a2) 
                                                                        
!     ----                                                              
!     Done                                                              
!     ----                                                              
                                                                        
      RETURN 
      END                                           
                                                                        
!=======================================================================
                                                                        
                                                                        
      SUBROUTINE CPUTIM (TIME) 
!                                                                       
      REAL*4 time_r4 
!                                                                       
                                                                        
!     The CPU_TIME xlf system subroutine returns the CPU time, in second
!     taken by the current process and, possibly, all the child processe
!     in all of the threads. A call to CPU_TIME will give the processor 
!     taken by the process from the start of the program. The time measu
!     only accounts for the amount of time that the program is actually 
!     and not the time that a program is suspended or waiting.          
                                                                        
      call cpu_time(time_r4) 
      time = time_r4 
                                                                        
!                                                                       
      RETURN 
!                                                                       
      END                                           
                                                                        
                                                                        
!=======================================================================
                                                                        
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
!#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))                    
!#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '                   
!                                                                       
      WRITE (IFILE) IARRAY 
!                                                                       
      RETURN 
!                                                                       
      END                                           
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
