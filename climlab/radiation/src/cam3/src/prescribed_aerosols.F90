#include <misc.h> 
#include <params.h>
#if ( defined SCAM )
#include <max.h>
#endif

module prescribed_aerosols

! Dummy version for CliMT -- aerosols disabled!

   implicit none

! naer_all is total number of species
! naer is number of species in climatology
! naer_all = naer + 1 (background "species") + 1 (volcanic)
  integer, public, parameter :: naer_all = 12
  integer, private, parameter :: naer = 10 

! indices to aerosol array (species portion)

  integer, public, parameter :: &
      idxSUL   =  1, &
      idxSSLT  =  2, &
      idxOCPHO =  7, &
      idxBCPHO =  8, &
      idxOCPHI =  9, &
      idxBCPHI = 10, &
      idxBG    = 11, &
      idxVOLC  = 12

! indices to sections of array that represent 
! groups of aerosols

  integer, public, parameter :: &
      idxDUSTfirst    = 3, &
      numDUST         = 4, &
      idxCARBONfirst = 7, &
      numCARBON      = 4

! compute stratospheric volcanic aerosol fields and forcing
  logical, public :: strat_volcanic   = .false.

end module prescribed_aerosols
