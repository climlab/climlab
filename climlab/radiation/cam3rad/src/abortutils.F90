#include <misc.h>

module abortutils

   private
   save

   public :: endrun

CONTAINS

subroutine endrun (msg)
!-----------------------------------------------------------------------
! Purpose:
!
! Abort the model for abnormal termination
!
! Author: CCM Core group
!
!-----------------------------------------------------------------------
! $Id: abortutils.F90,v 1.1 2009/02/12 16:41:42 rca Exp $
!-----------------------------------------------------------------------
#if (defined SPMD || defined COUP_CSM)
   use mpishorthand, only: MPI_COMM_WORLD
#endif
!!++CliMT   use shr_sys_mod, only: shr_sys_flush
!-----------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------
!
! Arguments
!
   character(len=*), intent(in), optional :: msg    ! string to be printed

   if (present (msg)) then
      write(6,*)'ENDRUN:', msg
   else
      write(6,*)'ENDRUN: called without a message string'
   end if

#if defined(NEC_SX)
   call mesput("ENDRUN", len("ENDRUN"), 1)
#elif defined(AIX)
   close(5)    ! needed to prevent batch jobs from hanging in xl__trbk
   call xl__trbk()
#endif

!!++CliMT   call shr_sys_flush( 6 )   ! Flush all output to standard output

#if (defined SPMD) || (defined COUP_CSM) 
! passing an argument of 1 to mpi_abort will lead to a STOPALL output 
! error code of 257
   call mpi_abort (MPI_COMM_WORLD, 1)  
#else
   call abort
#endif

end subroutine endrun
end module abortutils
