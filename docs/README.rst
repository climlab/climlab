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

The latest rendered version is always available online at `http://climlab.readthedocs.io`

Installation and Requirements
-----------------------------
Building the docs locally requires `sphinx` plus several extensions detailed in
the file `docs/environment.yml`.

You can use conda to set up a self-contained environment for building the docs.
From the `climlab` root directory, do this::

    conda env create --file docs/environment.yml
    conda activate climlab-docs

Now do a "soft install" of climlab into the new environment (this is necessary for building the docs, but the fortran components do not need to be compiled)::

    python setup.py install

Finally, build the docs from the `climlab/docs` directory with::

    make html

The documentation will be built in `climlab/docs/build/html`
and you can view them offline with a web browser.

When you are satisfied with your changes, you can deactivate the build environment with::

    conda deactivate

and (optionally) delete the build environment with::

    conda remove -n climlab-docs --all


Status
------
The documentation originated as a thesis project by Moritz Kreuzer,
focussing specifically on the EBM (energy balance model) code.

We are gradually expanding and updating the documentation to reflect the full capabilities of `climlab`.
It is a work-in-progress, and contributions to the documentation
as pull requests on `github <https://github.com/brian-rose/climlab>`_ are very welcome.
