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

Installing into a self-contained conda environment
--------------------------------------------------

To avoid issues with package conflicts, it's often best to work in self-contained environments.
This example installs climlab and jupyter along with all their dependencies in a fresh environment::

    conda create --name climlab-test --channel conda-forge climlab jupyter
    conda activate climlab-test

Installing on Google Colab
--------------------------

The following code will install climlab and its dependencies on `Google Colab`_::

    !pip install -q condacolab
    import condacolab
    condacolab.install()
    !conda install -c conda-forge climlab

Installing from source
----------------------

You can clone the source code repository with::

    git clone https://github.com/climlab/climlab.git

and from the ``climlab`` directory, do::

    python -m pip install . --no-deps -vv

Please see :ref:`Contributing to CLIMLAB` for more details.

About the compiled Fortran components
--------------------------------------------------

Climlab itself is pure Python and should work on any system.
As of version 0.8.0, all the Fortran code has been moved into external companion
packages `climlab-rrtmg`_, `climlab-cam3-radiation`_, and `climlab-emanuel-convection`_.

If you install climlab via conda-forge, these pre-compiled dependencies will be
installed automatically.

It is possible to install and run climlab without the compiled dependencies.
In this case you should then find that you can still::

    import climlab

and use most of the package. You will see warning messages about the missing components.

.. _conda: https://conda.io/docs/
.. _`Anaconda Python`: https://www.continuum.io/downloads
.. _`pypi repository`: https://pypi.python.org
.. _`climlab-rrtmg`: https://github.com/climlab/climlab-rrtmg
.. _`climlab-cam3-radiation`: https://github.com/climlab/climlab-cam3-radiation
.. _`climlab-emanuel-convection`: https://github.com/climlab/climlab-emanuel-convection
.. _`Google Colab`: https://colab.research.google.com

Source Code
=============

Stables releases as well as the current development version can be found on github:

  * `Stable Releases <https://github.com/climlab/climlab/releases>`_
  * `Development Version <https://github.com/climlab/climlab>`_


Dependencies
================

These are handled automatically if you install with conda_.

Required
------------
- Python (currently testing on versions 3.8, 3.9, 3.10, 3.11)
- numpy
- scipy
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
