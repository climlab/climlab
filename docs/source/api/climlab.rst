Reference
=================

This chapter documents the source code of the ``climlab`` package.
The focus is on the methods and functions that the user invokes
while using the package.
Nevertheless also the underlying code of the ``climlab`` architecture
has been documented for a comprehensive understanding and traceability.


.. automodule:: climlab

climlab subpackages
-----------

.. toctree::
    :maxdepth: 4

    climlab.convection
    climlab.domain
    climlab.dynamics
    climlab.model
    climlab.process
    climlab.radiation
    climlab.solar
    climlab.surface
    climlab.utils



Inheritance Diagram
-------------------

.. inheritance-diagram::
	climlab.domain.axis
	climlab.domain.domain
	climlab.domain.field
	climlab.dynamics.budyko_transport
	climlab.dynamics.diffusion
	climlab.model.ebm
	climlab.process.diagnostic
	climlab.process.energy_budget
	climlab.process.implicit
	climlab.process.process
	climlab.process.time_dependent_process
	climlab.radiation.aplusbt
	climlab.radiation.boltzmann
	climlab.radiation.insolation
	climlab.solar.insolation
	climlab.solar.orbital_cycles
	climlab.solar.orbital
	climlab.surface.albedo
   :parts: 1
   :private-bases:
