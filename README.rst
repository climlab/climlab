================
climlab
================

|docs| |JOSS| |DOI| |pypi| |Build Status| |coverage|

-----------------------------------------------------
 Python package for process-oriented climate modeling
-----------------------------------------------------

Author
--------------
| **Brian E. J. Rose**
| Department of Atmospheric and Environmental Sciences
| University at Albany
| brose@albany.edu


About climlab
--------------
``climlab`` is a flexible engine for process-oriented climate modeling.
It is based on a very general concept of a model as a collection of individual,
interacting processes. ``climlab`` defines a base class called ``Process``, which
can contain an arbitrarily complex tree of sub-processes (each also some
sub-class of ``Process``). Every climate process (radiative, dynamical,
physical, turbulent, convective, chemical, etc.) can be simulated as a stand-alone
process model given appropriate input, or as a sub-process of a more complex model.
New classes of model can easily be defined and run interactively by putting together an
appropriate collection of sub-processes.

Currently, ``climlab`` has out-of-the-box support and documented examples for

- Radiative and radiative-convective column models, with various radiation schemes:
    - RRTMG (a widely used radiative transfer code)
    - CAM3  (from the NCAR GCM)
    - Grey Gas
    - Simplified band-averaged models (4 bands each in longwave and shortwave)
- Convection schemes:
    - Emanuel moist convection scheme
    - Hard convective adjustment (to constant lapse rate or to moist adiabat)
- 1D Advection-Diffusion solvers
- Moist and dry Energy Balance Models
- Flexible insolation including:
  - Seasonal and annual-mean models
  - Arbitrary orbital parameters
- Boundary layer scheme including sensible and latent heat fluxes
- Arbitrary combinations of the above, for example:
    - 2D latitude-pressure models with radiation, horizontally-varying meridional diffusion, and fixed relative humidity


Installation
--------------

Installing pre-built binaries with conda (Mac OSX, Linux, and Windows)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By far the simplest and recommended way to install ``climlab`` is using conda_
(which is the wonderful package manager that comes with `Anaconda Python`_).

You can install ``climlab`` and all its dependencies with::

    conda install -c conda-forge climlab

Or (recommended) add ``conda-forge`` to your conda channels with::

    conda config --add channels conda-forge

and then simply do::

    conda install climlab

Binaries are available for OSX, Linux, and Windows.

Installing from source
~~~~~~~~~~~~~~~~~~~~~~
Consult the documentation_ for detailed instructions.

.. _conda: https://conda.io/docs/
.. _`Anaconda Python`: https://www.continuum.io/downloads
.. _`pypi repository`: https://pypi.python.org



Links
-----

-  HTML documentation: http://climlab.readthedocs.io/en/latest/intro.html
-  Issue tracker: http://github.com/brian-rose/climlab/issues
-  Source code: http://github.com/brian-rose/climlab
-  JOSS meta-paper: https://doi.org/10.21105/joss.00659


Dependencies
-----------------

These are handled automatically if you install with conda_.

Required
~~~~~~~~~~~~
- Python (currently testing on versions 3.7, 3.8, 3.9)
- numpy
- scipy
- attrdict
- future
- pooch (for remote data access and caching)
- xarray (for data handling)


*climlab will still run on Python 2.7 on some systems but we are no longer supporting this*

Recommended for full functionality
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- numba >=0.43.1 (used for acceleration of some components)

*Note that there is a bug in previous numba versions that caused a hanging condition in climlab under Python 3.*


Complete development environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To build from source and develop new code you will need some additional pieces

- pip
- gfortran (OSX or linux) or flang (Windows)
- pytest (to run the automated tests, important if you are developing new code)

To build the docs, yet another list of requirements

- sphinx
- ipython
- jupyter
- future
- sphinx_rtd_theme
- nbsphinx
- sphinxcontrib-bibtex
- numpydoc
- matplotlib

`Anaconda Python`_ is highly recommended and will provide everything you need.
See "Installing pre-built binaries with conda" above.


Documentation and Examples
--------------------------
Full user manual is available here_.

A rich and up-to-date collection of example usage can be found in Brian Rose's online textbook
`The Climate Laboratory`_.

Source notebooks for the `tutorials in the docs`_ can be found in the ``climlab/docs/source/courseware/`` directory of the source repo.

These are self-describing, and should run out-of-the-box once the package is installed, e.g:

``jupyter notebook Insolation.ipynb``


Release history
----------------------

Version 0.7.12 (released May 2021)
    New feature: spectral output from RRTMG (accompanied by a new tutorial)

Version 0.7.11 (released May 2021)
    Improvements to data file download and caching (outsourcing this to `pooch`_)

Version 0.7.10 (released April 2021)
    Improvements to docs and build.

Version 0.7.9 (released December 2020)
    Bug fixes and doc improvements.

Version 0.7.8 (released December 2020)
    Bug fixes.

Version 0.7.7 (released October 2020)
    Bug fixes.

Version 0.7.6 (released January 2020)
    Bug fixes, Python 3.8 compatibility, improvements to build and docs.

Version 0.7.5 (released July 2019)
    Bug fixes and improvements to continuous integration

Version 0.7.4 (released June 2019)
    New flexible solver for 1D advection-diffusion processes on non-uniform grids, along with some bug fixes.

Version 0.7.3 (released April 2019)
    Bug fix and changes to continuous integration for Python 2.7 compatibility

Version 0.7.2 (released April 2019)
    Improvements to surface flux processes, a new data management strategy, and improved documentation.

    Details:
      - ``climlab.surface.LatentHeatFlux`` and ``climlab.surface.SensibleHeatFlux`` are now documented, more consistent with the climlab API, and have new optional ``resistance`` parameters to reduce the fluxes (e.g. for modeling stomatal resistance)
      - ``climlab.surface.LatentHeatFlux`` now produces the diagnostic ``evaporation`` in kg/m2/s. ``climlab.convection.EmanuelConvection`` produces ``precipitation`` in the same units.
      - The previous ``PRECIP`` diagnostic (mm/day) in ``climlab.convection.EmanuelConvection`` is removed. This is a BREAKING CHANGE.
      - Data files have been removed from the climlab source repository. All data is now accessible remotely. climlab will attempt to download and cache data files upon first use.
      - ``climlab.convection.ConvectiveAdjustement`` is now accelerated with ``numba`` if it is available (optional)

Version 0.7.1 (released January 2019)
    Deeper xarray integration, include one breaking change to ``climlab.solar.orbital.OrbitalTable``, Python 3.7 compatibility, and minor enhancements.

    Details:
      - Removed ``climlab.utils.attr_dict.AttrDict`` and replaced with AttrDict package (a new dependency)
      - Added ``xarray`` input and output capabilities for ``climlab.solar.insolation.daily_insolation()``
      - ``climlab.solar.orbital.OrbitalTable`` and ``climlab.solar.orbital.long.OrbitalTable`` now return ``xarray.Dataset`` objects containing the orbital data.
      - The ``lookup_parameter()`` method was removed in favor of using built-in xarray interpolation.
      - New class ``climlab.process.ExternalForcing()`` for arbitrary externally defined tendencies for state variables.
      - New input option ``ozone_file=None`` for radiation components, sets ozone to zero.
      - Tested on Python 3.7. Builds will be available through conda-forge.

Version 0.7.0 (released July 2018)
    New functionality, improved documentation_, and a few breaking changes to the API.

    Major new functionality includes `convective adjustment to the moist adiabat <http://climlab.readthedocs.io/en/latest/api/climlab.convection.convadj.html>`_ and `moist EBMs with diffusion on moist static energy gradients <http://climlab.readthedocs.io/en/latest/api/climlab.model.ebm.html>`_.

    Details:

    - ``climlab.convection.ConvectiveAdjustement`` now allows non-constant critical lapse rates, stored in input parameter ``adj_lapse_rate``.
        - New switches to implement automatic adjustment to **dry** and **moist** adiabats (pseudoadiabat)
    - ``climlab.EBM()`` and its daughter classes are significantly reorganized to better respect CLIMLAB principles:
        - Essentially all the computations are done by subprocesses
        - SW radiation is now handled by ``climlab.radiation.SimpleAbsorbedShortwave`` class
        - Diffusion and its diagnostics now handled by ``climlab.dynamics.MeridionalHeatDiffusion`` class.
        - Diffusivity can be altered at any time by the user, e.g. during timestepping
        - Diffusivity input value ``K`` in class ``climlab.dynamics.MeridionalDiffusion`` is now specified in physical units of m2/s instead of (1/s). This is consistent with its parent class ``climlab.dynamics.Diffusion``.
    - A new class ``climlab.dynamics.MeridionalMoistDiffusion`` for the moist EBM (diffusion down moist static energy gradient)
    - Tests that require compiled code are now marked with ``pytest.mark.compiled`` for easy exclusion during local development

    Under-the-hood changes include

    - Internal changes to the timestepping; the ``compute()`` method of every subprocess is now called explicitly.
    - ``compute()`` now always returns tendency dictionaries

Version 0.6.5 (released April 2018)
    Some improved documentation, associated with publication of a meta-description paper in JOSS.

Version 0.6.4 (released February 2018)
    Some bug fixes and a new ``climlab.couple()`` method to simplify creating complete models from components.

Version 0.6.3 (released February 2018)
    Under-the-hood improvements to the Fortran builds which enable successful builds on a wider variety of platforms (incluing Windows/Python3).

Version 0.6.2 (released February 2018)
    Introduces the Emanuel moist convection scheme, support for asynchonous coupling, and internal optimzations.

Version 0.6.1 (released January 2018)
    Provides basic integration with xarray_
    (convenience methods for converting climlab objects into ``xarray.DataArray`` and ``xarray.Dataset`` objects)

Version 0.6.0 (released December 2017)
    Provides full Python 3 compatibility, updated documentation, and minor enhancements and bug fixes.

Version 0.5.5 (released early April 2017)
    Finally provides easy binary distrbution with conda_

Version 0.5.2 (released late March 2017)
    Many under-the-hood improvements to the build procedure,
    which should make it much easier to get `climlab` installed on user machines.
    Binary distribution with conda_ is coming soon!

Version 0.5 (released March 2017)
    Bug fixes and full functionality for the RRTMG radiation module,
    an improved common API for all radiation modules, and better documentation.

Version 0.4.2 (released January 2017)
    Introduces the RRTMG radiation scheme,
    a much-improved build process for the Fortran extension,
    and numerous enhancements and simplifications to the API.

Version 0.4 (released October 2016)
    Includes comprehensive documentation, an automated test suite,
    support for latitude-longitude grids, and numerous small enhancements and bug fixes.

Version 0.3 (released February 2016)
    Includes many internal changes and some backwards-incompatible changes
    (hopefully simplifications) to the public API.
    It also includes the CAM3 radiation module.

Version 0.2 (released January 2015)
    The package and its API was completely redesigned around a truly object-oriented
    modeling framework in January 2015.

    It was used extensively for a graduate-level climate modeling course in Spring 2015:
    http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/

    Many more examples are found in the online lecture notes for that course:
    http://nbviewer.jupyter.org/github/brian-rose/ClimateModeling_courseware/blob/master/index.ipynb

Version 0.1
    The first versions of the code and notebooks were originally developed in winter / spring 2014
    in support of an undergraduate course at the University at Albany.

    See the original course webpage at
    http://www.atmos.albany.edu/facstaff/brose/classes/ENV480_Spring2014/


The documentation_ was first created by Moritz Kreuzer
(Potsdam Institut for Climate Impact Research) as part of a thesis project in Spring 2016.

.. _documentation: http://climlab.readthedocs.io
.. _xarray: http://xarray.pydata.org/en/stable/
.. _pooch: https://www.fatiando.org/pooch/latest/index.html
.. _`tutorials in the docs`: https://climlab.readthedocs.io/en/latest/tutorial.html
.. _here: http://climlab.readthedocs.io
.. _`The Climate Laboratory`: https://brian-rose.github.io/ClimateLaboratoryBook/


Contact and Bug Reports
-----------------------
Users are strongly encouraged to submit bug reports and feature requests on
github at
https://github.com/brian-rose/climlab


License
---------------
This code is freely available under the MIT license.
See the accompanying LICENSE file.

.. |JOSS| image:: http://joss.theoj.org/papers/10.21105/joss.00659/status.svg
   :target: https://doi.org/10.21105/joss.00659
.. |pypi| image:: https://badge.fury.io/py/climlab.svg
   :target: https://badge.fury.io/py/climlab
.. |Build Status| image:: https://github.com/brian-rose/minimalf2py/actions/workflows/build-and-test.yaml/badge.svg
    :target: https://github.com/brian-rose/climlab/actions/workflows/build-and-test.yml
.. |coverage| image:: https://codecov.io/github/brian-rose/climlab/coverage.svg?branch=main
   :target: https://codecov.io/github/brian-rose/climlab?branch=main
.. |DOI| image:: https://zenodo.org/badge/24968065.svg
   :target: https://zenodo.org/badge/latestdoi/24968065
.. |docs| image:: http://readthedocs.org/projects/climlab/badge/?version=latest
   :target: http://climlab.readthedocs.io/en/latest/intro.html
   :alt: Documentation Status

=======


Support
-----------------
Development of ``climlab`` is partially supported by the National Science Foundation under award AGS-1455071 to Brian Rose.

Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.
