#include <misc.h>
#include <params.h>

module ppgrid

!----------------------------------------------------------------------- 
! 
! Purpose: 
! Initialize physics grid resolution parameters
!  for a chunked data structure
! 
! Author: 
! 
!-----------------------------------------------------------------------

! Grid point resolution parameters

   integer pcols      ! number of columns (max)
   integer pver       ! number of vertical levels
   integer pvermx     ! number of subsurface levels
   integer pverp      ! pver + 1

   parameter (pcols  = 1)
   parameter (pver   = PLEV)
   parameter (pvermx = 4)
   parameter (pverp  = pver + 1  )
!
! start, end indices for chunks owned by a given MPI task
! (set in phys_grid_init).
!
   integer :: begchunk = 0            ! 
   integer :: endchunk = -1           ! 

end module ppgrid
