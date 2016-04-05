.. highlight:: rst

Download
========

Code
----

Stables releases as well as the current development version can be found on github:

  * `Stable Releases <https://github.com/brian-rose/climlab/releases>`_
  * `Development Version <https://github.com/brian-rose/climlab>`_


Dependencies
------------
`climlab` is written in `Python 2.7 <https://www.python.org/downloads/>`_ and requires following `Python` packages to run:

  * `Numpy <http://www.numpy.org/>`_
  * `Scipy <https://www.scipy.org/>`_
  * `NetCDF4 <https://unidata.github.io/netcdf4-python/>`_

**Optional packages:**

  * `Jupyter <http://jupyter.org/>`_ (to run Jupyter notebooks containing tutorials and introduction for climlab)
  * `Matplotlib <http://matplotlib.org/>`_ (plotting libary)

The packages have to be installed on your machine. They can either be downloaded, compiled and installed individually. 

Otherwise Python distributions like `Anaconda <https://www.continuum.io/downloads>`_ or `Enthought Canopy <https://www.enthought.com/products/canopy/>`_ can be used which already include many popular Python packages.

Setup environment with Anaconda
###############################

An example is given here how to set up a python environment with `Anaconda <https://www.continuum.io/downloads>`_.

A new environment named ``climlab_env`` with all above packages is created like this:

	.. code-block:: console

		$ conda create --name climlab_env numpy scipy netcdf4 jupyter matplotlib
       
	All new packages which will be installed are displayed including their version number. Press ``y`` to proceed.
	
	After the environment is build, it can be activated with

	.. code-block:: console

		$ source activate climlab_env

	and can be deactivated with

	.. code-block:: console

		$ source deactivate

	
	To install climlab in the new environment follow the steps below.        

Installation
------------

Stable Release
##############

	With the Python package `pip <http://www.pip-installer.org/>`_ which collects the current version from the `Python Package Index <https://pypi.python.org/pypi>`_, climlab can be easily installed on a machine through the terminal command

		.. code-block:: console

			$ pip install climlab

Development Version
###################

	Otherwise the package can be downloaded from the above referred link and installed manually through running from the package directory

		.. code-block:: console

			$ python setup.py install

	for a regular system wide installation.

	In case you want to develop new code, run following command (which also has an uninstall option):

		.. code-block:: console

			$ python setup.py develop


