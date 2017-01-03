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
