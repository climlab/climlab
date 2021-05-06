.. highlight:: rst

Installation
============

Installing pre-built binaries with conda (Mac OSX, Linux, and Windows)
----------------------------------------------------------------------
By far the simplest and recommended way to install ``climlab`` is using conda_
(which is the wonderful package manager that comes with `Anaconda Python`_).

You can install CLIMLAB and all its dependencies with::

    conda install -c conda-forge climlab

Or (recommended) add ``conda-forge`` to your conda channels with::

    conda config --add channels conda-forge

and then simply do::

    conda install climlab

Binaries are available for OSX, Linux, and Windows.

Installing from source
----------------------

*This will only work if you have a properly configured Fortran compiler installed and available.*

You can clone the source code repository with::

    git clone https://github.com/brian-rose/climlab.git

and, if your system is properly configured *(it probably isn't right out of the box)*,
from the ``climlab`` directory, do::

    python -m pip install . --no-deps -vv

Please see :ref:`Contributing to CLIMLAB` for details about how to set up a working build environment and building from source.

Installing from source without a Fortran compiler
-------------------------------------------------

Many parts of CLIMLAB are written in pure Python and should work on any system. Fortran builds are necessary for the Emanuel convection scheme and the RRTMG and CAM3 radiation schemes.

If you obtain the source code repository and do this from the repository root::

    python setup.py install

You should then find that you can still::

    import climlab

and use most of the package. You will see warning messages about the missing components.

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
- Python (currently testing on versions 3.7, 3.8, 3.9)
- numpy
- scipy
- attrdict
- future
- pooch (for remote data access and caching)
- xarray (for data handling)

*climlab will still run on Python 2.7 on some systems but we are no longer supporting this*

Recommended for full functionality
----------------------------------
- numba (used for acceleration of some components)
- pytest (to run the automated tests, important if you are developing new code)

`Anaconda Python`_ is highly recommended and will provide everything you need.
See "Installing pre-built binaries with conda" above.
