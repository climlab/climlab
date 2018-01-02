================
climlab
================

|docs| |DOI| |pypi| |Build Status| |coverage|

----------
 Python package for process-oriented climate modeling
----------

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
- Diffusive energy balance models
- Seasonal and steady-state models
- Arbitrary combinations of the above, for example:
    - 2D latitude-pressure models with radiation, horizontal diffusion, and fixed relative humidity
- Orbital / insolation calculations
- Boundary layer sensible and latent heat fluxes


Installation
--------------

Installing pre-built binaries with conda (Mac OSX, Linux, and Windows)
~~~~~~~~~~~~~~~~~~~~~~
By far the simplest and recommended way to install ``climlab`` is using conda_
(which is the wonderful package manager that comes with `Anaconda Python`_).

You can install ``climlab`` and all its dependencies with::

    conda install -c conda-forge climlab

Or (recommended) add ``conda-forge`` to your conda channels with::

    conda config --add channels conda-forge

and then simply do::

    conda install climlab

Binaries are available for OSX, Linux, and Windows.
You may need to update your ``numpy`` if you are using are using a version prior to 1.11

Installing from source
~~~~~~~~~~~~~~~~~~~~~~
If you do not use conda, you can install ``climlab`` from source with::

    pip install climlab

(which will download the latest stable release from the `pypi repository`_ and trigger the build process.)

Alternatively, clone the source code repository with::

    git clone https://github.com/brian-rose/climlab.git

and, from the ``climlab`` directory, do::

    python setup.py install

You will need a Fortran compiler on your system.
The build has been tested with both gcc/gfortran and ifort (Linux)

Installing from source without a Fortran compiler
~~~~~~~~~~~~~~~~~~~~~~
Many parts of ``climlab`` are written in pure Python and should work on any system.
Fortran builds are necessary for the RRTMG and CAM3 radiation schemes.
If you follow the instructions for installing from source (above) without a valid Fortran compiler,
you should find that you can still::

    import climlab

and use most of the package. You will see warning messages about the missing radiation components.

.. _conda: https://conda.io/docs/
.. _`Anaconda Python`: https://www.continuum.io/downloads
.. _`pypi repository`: https://pypi.python.org



Links
-----

-  HTML documentation: http://climlab.readthedocs.io/en/latest/intro.html
-  Issue tracker: http://github.com/brian-rose/climlab/issues
-  Source code: http://github.com/brian-rose/climlab


Dependencies
-----------------

These are handled automatically if you install with conda_.

Required
~~~~~~~~~~~~
- Python 2.7, 3.5, or 3.6 (as of version 0.6.0)
- ``numpy``
- ``scipy``
- ``netCDF4`` Python package (for data i/o)

Recommended for full functionality
~~~~~~~~~~~~
- ``numba`` (used for acceleration of some components)
- ``xarray`` (convenience methods to convert model data to ``xarray.DataArray`` format are available)
- ``pytest`` (to run the automated tests, important if you are developing new code)

`Anaconda Python`_ is highly recommended and will provide everything you need.
See "Installing pre-built binaries with conda" above.


Documentation and Examples
------------------
Full user manual is available here_.

The directory ``climlab/courseware/`` also contains a collection of Jupyter
notebooks (*.ipynb) used for teaching some basics of climate science,
and documenting use of the ``climlab`` package.
These are self-describing, and should all run out-of-the-box once the package is installed, e.g:

``jupyter notebook Insolation.ipynb``


.. _here: http://climlab.readthedocs.io


History
----------------------
The first versions of the code and notebooks were originally developed in winter / spring 2014
in support of an undergraduate course at the University at Albany.
See the original course webpage at
http://www.atmos.albany.edu/facstaff/brose/classes/ENV480_Spring2014/

The package and its API was completely redesigned around a truly object-oriented
modeling framework in January 2015.

It was used extensively for a graduate-level climate modeling course in Spring 2015:
http://www.atmos.albany.edu/facstaff/brose/classes/ATM623_Spring2015/
Many more examples are found in the online lecture notes for that course:
http://nbviewer.jupyter.org/github/brian-rose/ClimateModeling_courseware/blob/master/index.ipynb

Version 0.3 was released in February 2016. It includes many internal changes and
some backwards-incompatible changes (hopefully simplifications) to the public API.
It also includes the CAM3 radiation module.

Version 0.4 was released in October 2016. It includes comprehensive documentation,
an automated test suite, support for latitude-longitude grids, and numerous small enhancements and bug fixes.

Version 0.4.2 (released January 2017) introduces the RRTMG radiation scheme,
a much-improved build process for the Fortran extension,
and numerous enhancements and simplifications to the API.

Version 0.5 (released March 2017) provides bug fixes and full functionality for the RRTMG module,
an improved common API for all radiation modules, and better documentation.

Version 0.5.2 (released late March 2017) provides many under-the-hood improvements to the build procedure,
which should make it much easier to get `climlab` installed on user machines. Binary distribution with conda_ is coming soon!

Version 0.5.5 (released early April 2017) finally provides easy binary distrbution with conda_

Version 0.6.0 (released December 2017) provides full Python 3 compatibility, updated documentation, and minor enhancements and bug fixes.

Version 0.6.1 (released January 2018) provides basic integration with xarray_
(convenience methods for converting climlab objects into ``xarray.DataArray`` and ``xarray.Dataset`` objects)

The documentation_ was first created by Moritz Kreuzer (Potsdam Institut for Climate Impact Research) as part of a thesis project in Spring 2016.

.. _documentation: http://climlab.readthedocs.io
.. _xarray: http://xarray.pydata.org/en/stable/


Contact and Bug Reports
----------------------
Users are strongly encouraged to submit bug reports and feature requests on
github at
https://github.com/brian-rose/climlab


License
---------------
This code is freely available under the MIT license.
See the accompanying LICENSE file.

.. |pypi| image:: https://badge.fury.io/py/climlab.svg
   :target: https://badge.fury.io/py/climlab
.. |Build Status| image:: https://travis-ci.org/brian-rose/climlab.svg?branch=master
    :target: https://travis-ci.org/brian-rose/climlab
.. |coverage| image:: https://codecov.io/github/brian-rose/climlab/coverage.svg?branch=master
   :target: https://codecov.io/github/brian-rose/climlab?branch=master
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
