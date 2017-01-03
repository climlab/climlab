# New CAM3Radiation implementation for climlab

## Brian Rose, January 2017

We need a CAM3Radiation module that is:

- compiled without knowing grid dimensions
- copyable from an interactive Python session
- works with arbitrary number of vertical levels
- can have arbitrary number of individual objects present in memory at any time

The wrapper code should also be simplified and vestiges of CliMT cleaned up

The fortran shared object (compiled once at install time) needs use dynamic
arrays for storage since we don't know number of vertical levels at compile time

It's important to keep the actual *numerical calculation* as distinct as possible
from the storage and initialization. That should be done at Python level if possible.

The shared object should basically take all information about the domain
including info about absorptivity
and just compute radiative fluxes.

This requires separating out some more of the initialization out of the Fortran code.

**The fundamental problem is that any data attached to the Fortran modules
is effectively global data belonging to the module imported into Python**

This means trying to re-import the same module and rewrite the data to have
more than one independent model running in memory simultaneously is **not**
going to work!

So it seems like a very careful teasing apart of the data structures
from the numerical code is necessary...

Presently in `crm.F90` we have the following calls in every iteration
before the actual calls to SW and LW radiation:

```
! Initialize rad routines
call radsw_init(gravit)
call radlw_init(gravit, stebol)
call radae_init(gravit, epsilo, stebol, pstd, mwdry, mwco2, mwo3)
```

But the first two of these are just doing some trivial setting of constants

Anyway the main calls that actually invoke the radiative transfer calculation are
`call radcswmx` and `call radclwmx`.

Need to reorganize the code so that these calls are essentially free-standing
and don't rely on any global module data.

### Removing dependence on ppgrid.F90 module

Basically I need to rewrite all subroutines to accept grid dimensions (pcols, pver, pverp)
as input arguments rather than using the ppgrid module. Yes? That should be straightforward.
There will be more work to do to figure out storage of abs/ems data.

... This is done now! Builds and runs -- but for now it *only* works with 30 levels

### Identifying and removing *all* state and size-dependent data storage

- When the module is initialized, the abs / ems data must be read from netcdf file
- This is universal and independent of grid dimensions
- In fortran code this info is stored in arrays:
  - `ah2onw`, `eh2onw`, `ah2ow`, `ln_ah2ow`, `cn_ah2ow`, `ln_eh2ow`, `cn_eh2ow`
  - Currently these are defined in `absems.F90` as module global data
- This data is used on the model grid through arrays
```
real(r8), public, allocatable, target :: abstot_3d(:,:,:,:) ! Non-adjacent layer absorptivites
real(r8), public, allocatable, target :: absnxt_3d(:,:,:,:) ! Nearest layer absorptivities
real(r8), public, allocatable, target :: emstot_3d(:,:,:)   ! Total emissivity
```
also currently defined in `absems.F90` as module global data
- Where and when do these values get set?
  - A pointer to emstot_3d is passed to `radems` inside `subroutine radclwmx`
  - Values are set in this subroutine (in `radae.F90`) using the abs/ems data in arrays `ah2onw`, etc.
  - Similarly...  pointers to abstot_3d and absnxt_3d are passed to `radabs` inside `subroutine radclwmx`
  - It is here that the data `ah2onw` etc are used.
- So basically the code in `radae.F90` wants to be a free-standing module that computes absorptivities and emissivity for a particular grid and model state.
- This is the part that needs the data in the netCDF file.
- Currently we have the data being read in and the `ah2onw` etc. arrays populated at import time. That's good.
- What we need to do is make the `abstot_3d` etc. arrays local and not global module arrays.
- Why not declare them in the driver?
- Actually these arrays are *only* used within the LW code in `subroutine radclwmx`
- So it makes sense to define them locally in that subroutine
(could be a performance penalty but whatever)

... This is now done! Compiles are runs with arbirary levels! And I can freely copy and have multiple instances on different grids and it seems to work!


### Still to do:

- Rationalize physical constants and basic thermo (saturation)
- Radiation code should be using same constants as the rest of climlab
- Get rid of vestigal parallelism in the CAM3 code. It's just cruft.
- If ever we implement parallelism in CLIMLAB it will be operating at a higher abstraction level
