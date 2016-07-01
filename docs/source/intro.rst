.. highlight:: rst

.. _Introduction:

Introduction
============

What is climlab?
----------------

`climlab` is a flexible engine for process-oriented climate modeling.
It is based on a very general concept of a model as a collection of individual,
interacting processes. `climlab` defines a base class called `Process`, which
can contain an arbitrarily complex tree of sub-processes (each also some
sub-class of `Process`). Every climate process (radiative, dynamical,
physical, turbulent, convective, chemical, etc.) can be simulated as a stand-alone
process model given appropriate input, or as a sub-process of a more complex model.
New classes of model can easily be defined and run interactively by putting together an
appropriate collection of sub-processes.

Most of the actual computation for simpler model components use vectorized
``numpy`` array functions. It should run out-of-the-box on a standard scientific
Python distribution, such as ``Anaconda`` or ``Enthought Canopy``.


What's new in version 0.3?
--------------------------

New in version 0.3, `climlab` now includes Python wrappers for more
numerically intensive processes implemented in Fortran code (specifically the
CAM3 radiation module). These require a Fortran compiler on your system,
but otherwise have no other library dependencies.  `climlab` uses a compile-on-demand
strategy. The compiler is invoked automatically as necessary when a new process
is created by the user.


Implementation of models
------------------------

Currently, `climlab` has out-of-the-box support and documented examples for:

- 1D radiative and radiative-convective single column models, with various radiation schemes:

    - Grey Gas

    - Simplified band-averaged models (4 bands each in longwave and shortwave)

    - One GCM-level radiation module (CAM3)

- 1D diffusive energy balance models

- Seasonal and steady-state models

- Arbitrary combinations of the above, for example:

    - 2D latitude-pressure models with radiation, horizontal diffusion, and fixed relative humidity

- orbital / insolation calculations

- boundary layer sensible and latent heat fluxes

.. note::

	For more details about the implemented Energy Balance Models, see the :ref:`models` chapter.


Documentation
-------------

The documentation is a work in progress! Currently there is good coverage and example usage
for the Energy Balance Models. Comprehensive tutorials for other model types will be coming soon.
