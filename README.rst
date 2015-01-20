================
climlab
================
----------
 Python package for object-oriented climate modeling
----------

Author
=============
**Brian E. J. Rose**

Department of Atmospheric and Environmental Sciences

University at Albany

brose@albany.edu

Installation
----------------
``python setup.py``

    or, if you are developing new code

``python setup.py develop``


About climlab
--------------
climlab is a flexible engine for modeling climate processes.
It is based on a very general concept of a model as a collection of individual, 
interacting processes. climlab defines a base class called ``Process``, which
can contain an arbitrarily complex tree of sub-processes (each also some 
sub-class of ``Process``.) Every climate process (radiative, dynamical, 
physical, turbulent, convective, chemical, etc.) can be simulated as a stand-alone
process model, or as a sub-process of a more complex model.

Most of the actual computation uses standard vectorized ``numpy`` functions. 
It should out-of-the-box on a standard scientific Python distribution.
Future versions of ``climlab`` will provide hooks to compiled Fortran code for 
more numerically intensive processes.

Currently, ``climlab`` has out-of-the-box support for 
- 1D grey-radiation and radiative-convective single column models
- 1D diffusive energy balance models
- Seasonal and steady-state models
- orbital / insolation calculations.

Example usage
------------------
The directory ``climlab/courseware/`` contains a collection of IPython notebooks (*.ipynb)
used for teaching some basics of climate science, 
and documenting use of the climlab package.
These are self-describing, and should all run out-of-the-box once the package is installed, e.g:
``ipython notebook Insolation.ipynb``

History
----------------------
The first versions of the code and notebooks were originally developed in winter / spring 2014
in support of an undergraduate course at the University at Albany.
See the original course webpage at
http://www.atmos.albany.edu/facstaff/brose/classes/ENV480_Spring2014/

The package and its API was completely redesigned around a truly object-oriented 
modeling framework in January 2015.

License
---------------
This code is freely available under the MIT license.
See the accompanying LICENSE file.
