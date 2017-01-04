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
! climlab -- may as well add these here, they are needed in radlw.F90
   integer pverp2     ! pver + 2
   integer pverp3     ! pver + 3
   integer pverp4     ! pver + 4
! end

! climlab -- let pver be set dynamically
   parameter (pcols  = 1)
!   parameter (pver   = PLEV)
   parameter (pvermx = 4)
!   parameter (pverp  = pver + 1  )
!
! start, end indices for chunks owned by a given MPI task
! (set in phys_grid_init).
!
   integer :: begchunk = 0            !
   integer :: endchunk = -1           !

! climlab -- adding initialization function here
contains
   subroutine set_pver(num_lev)
      integer num_lev

      pver = num_lev
      pverp = pver + 1
      pverp2 = pver + 2
      pverp3 = pver + 3
      pverp4 = pver + 4
   end subroutine set_pver


end module ppgrid
