.. highlight:: rst

Installation
============

Installing pre-built binaries with conda (Mac OSX, Linux, and Windows)
--------------------------
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
----------------------
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
-------------------------------------

Many parts of ``climlab`` are written in pure Python and should work on any system.
Fortran builds are necessary for the RRTMG and CAM3 radiation schemes.
If you follow the instructions for installing from source (above) without a valid Fortran compiler,
you should find that you can still::

    import climlab

and use most of the package. You will see warning messages about the missing radiation components.

.. _conda: https://conda.io/docs/
.. _`Anaconda Python`: https://www.continuum.io/downloads
.. _`pypi repository`: https://pypi.python.org




Source Code
=============

Stables releases as well as the current development version can be found on github:

  * `Stable Releases <https://github.com/brian-rose/climlab/releases>`_
  * `Development Version <https://github.com/brian-rose/climlab>`_


Dependencies
================

These are handled automatically if you install with conda_.

Required
------------
- Python 2.7, 3.5, or 3.6 (as of version 0.6.0)
- ``numpy``
- ``scipy``
- ``netCDF4`` Python package (for data i/o)

Recommended for full functionality
---------------
- ``numba`` (used for acceleration of some components)
- ``pytest`` (to run the automated tests, important if you are developing new code)

`Anaconda Python`_ is highly recommended and will provide everything you need.
See "Installing pre-built binaries with conda" above.
