=====================
climlab-documentation
=====================

---------------------------------------------------
Sphinx documentation for the climlab python package
---------------------------------------------------

Authors
------
| **Brian E. J. Rose**
| University at Albany
| brose@albany.edu
|
| **Moritz Kreuzer**
| Potsdam Institute for Climate Impact Research
| kreuzer@pik-potsdam.de

About
-----
Sphinx-based documentation for the `climlab` (https://github.com/brian-rose/climlab) package.

Installation
------
To build the html documenation locally, from the `climlab/docs` directory run
```
make html
```
The documentation will be built in `climlab/docs/build/html`

Or just read the latest rendered version online at `http://climlab.readthedocs.io`

Requires
-----
Building the docs locally requires `sphinx` plus several extensions. See the file `environment.yml`


Status
------
The documentation originated as a thesis project by Moritz Kreuzer,
focussing specifically on the EBM (energy balance model) code.

We are gradually expanding and updating the documentation to reflect the full capabilities of `climlab`.
It is a work-in-progress, and contributions to the documentation
as pull requests on `github <https://github.com/brian-rose/climlab>`_ are very welcome.
