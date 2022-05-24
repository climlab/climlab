.. highlight:: rst

.. _`Contributing to CLIMLAB`:


Contributing to CLIMLAB
=======================

This is an open project, and contributions of all kinds are welcome!

Here are some guidelines for how to get involved.


Usage in publications, teaching, etc.
-------------------------------------

If you use CLIMLAB in any way for published research or theses, online teaching materials, or anything else, we would appreciate hearing about it. Our goal is to maintain a list of links and references to use cases. This is essential information for our funders (NSF) but will also be a great resource for new users looking to find out more about what you can do with CLIMLAB.

The best way to report usage is through this `open issue on the CLIMLAB github page`_. If you're not a github users you can report usage directly to Brian Rose (see `Contact page`_).

For publications, please cite the `CLIMLAB description paper in JOSS`_. The full citation is:

  Rose, (2018). CLIMLAB: a Python toolkit for interactive, process-oriented climate modeling. Journal of Open Source Software, 3(24), 659, https://doi.org/10.21105/joss.00659


Reporting bugs, issues, new feature requests, and documentation problems
------------------------------------------------------------------------

These can all be raised as new issues at <https://github.com/climlab/climlab/issues>

If you are reporting a bug, try to include:

- Your operating system name and version
- The Python and CLIMLAB versions you are using
- Minimal code to reproduce the bug

If you aren't sure about any of this, please just post your issue anyway and we will assist.

Feel free to point out any inaccuracies or omissions in the documentation_ here as well.


Seeking help and support
------------------------

Although CLIMLAB is offered to the community "as-is", we are very interested in
helping people actually use it for scientific purposes.

Have a question about how to do something with CLIMLAB? First, make sure you've
perused the documentation_ and the issue tracker at <https://github.com/climlab/climlab/issues>.
Also look through published examples including the online book `The Climate Laboratory`_.

Then, feel free to ask questions by opening a new issue at <https://github.com/climlab/climlab/issues>.
This requires a free github account but is the best way to engage the community for answers.
We will do our best to respond. If the functionality you're looking for doesn't
yet exist, we'll probably encourage you to get involved in developing the next big feature.


Contributing bug fixes and new features
---------------------------------------

We are thrilled to have any and all help.
You may want to browse through <https://github.com/climlab/climlab/issues>
to see if there is any low-hanging fruit already identified.

Contributions will happen through Pull Requests on github.
You will need a free github account. Here's how to get started:

1. Follow `these instructions`_ to fork the CLIMLAB repo at <https://github.com/climlab/climlab>,
clone it on your local machine, and keep your local main branch synced with the main repo.

2. Don't make any commits on your local main branch. Instead open a feature branch for every new development task::

    git checkout -b cool_new_feature

(choose a more descriptive name for your new feature).

3. Work on your new feature, using ``git add`` to add your changes.

4. Build and Test your modified code! See below for instructions. Make sure to add new tests for your cool new feature.

5. When your feature is complete and tested, commit your changes::

    git commit -m 'I made some cool new changes'

and push your branch to github::

    git push origin cool_new_feature

6. At this point, you go find your fork on github and create a `pull request`_.
Clearly describe what you have done in the comments. We will gladly merge any pull requests that fix outstanding issues with the code or documentation.
If you are adding a new feature, it is important to also add appropriate tests of the new feature to the automated test suite.
If you don't know how to do this, submit your pull request anyway and we will assist.

7. After your pull request is merged, you can switch back to the main branch, rebase, and delete your feature branch. You will find your improvements are incorporated into CLIMLAB::

    git checkout main
    git fetch upstream
    git rebase upstream/main
    git branch -d cool_new_feature


Building CLIMLAB from source
----------------------------

As of version 0.8.0, all the Fortran code has been moved into external companion
packages `climlab-rrtmg`_, `climlab-cam3-radiation`_, and `climlab-emanuel-convection`_.
You no longer need a Fortran compiler to build climlab from source.

Here are some basic instructions for setting up an environment to build and test climlab.

Using conda to set up a complete build environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Included with the CLIMLAB source repo are some YAML files that describe complete
conda environments for building, testing and running the code.
We can use these to quickly create an environment with everything we need.
We then build and test manually within this environment.

First, create and activate a test environment with your desired python version, for example::

    conda create --name test_env python=3.10 --channel conda-forge
    conda activate test_env

Next, update the test environment with all the necessary build dependencies.
Do this from the top level of the CLIMLAB source repo::

    conda env update --file environment.yml

Alternatively, if you don't need to specify the Python version and just want to use the default,
you can create the complete environment in a single step like this::

    conda env create --file environment.yml
    conda activate test_env

Either way, you are now ready to build from source and install in this new environment::

    python -m pip install . --no-deps -vv

You can now test-drive your new build. To run the full test suite, you can do this (from any directory other than the CLIMLAB repo)::

    pytest -v --pyargs climlab

All tests should report ``PASSED``.

When you are done with your test environment, you can safely deactivate and delete it with::

    conda deactivate
    conda env remove --name test_env


Testing
-------

CLIMLAB has an extensive set of tests designed to work with `pytest`_. The test code is found in the ``climlab/tests`` directory inside the source repo.

To run the full set of tests on the currently installed version of CLIMLAB, you can always do this (from any directory except the CLIMLAB repo)::

    pytest -v --pyargs climlab

All tests should report ``PASSED``.

If you are developing new code, it is useful (and quicker) to run tests directly
from the source code directory. From the ``climlab`` root directory, do the following::

    pytest -v

which excludes the tests marked as requiring the compiled components. Again, look for all tests to report ``PASSED``. For more details see the `pytest`_ documentation.

If you're working on a new feature, we suggest that in the spirit of good software design you `write the new test before you write the new code`_! But we will be happy to help and discuss on github.



Contributing improved documentation
-----------------------------------

The documentation_ is generated with Sphinx from docstrings in the source code itself,
along with a small collection of ReStructuredText_ (.rst) files.
You can help improve the documentation!
This is often the simplest way to get involved with any open source project.

- Create and checkout a new feature branch as described above.
- Edit doctrings and/or .rst files in ``climlab/docs/``

- Use conda to set up a complete build environment for the docs! From the ``climlab`` root directory, do this::

    conda env create --file docs/environment.yml
    conda activate climlab-docs

- Now install climlab into the new environment (this is necessary for building the docs)::

    python -m pip install . --no-deps -vv

- Finally, build the docs from the ``climlab/docs`` directory with::

    make html

- The new and improved docs should now be available locally in the ``climlab/docs/build/html`` directory. Check them out in your web browser.
- Once you are satisfied, commit changes as described above and submit a new Pull Request describing your changes.
- You can deactivate the build environment with::

    conda deactivate

and (optionally) delete the build environment with::

    conda env remove --name climlab-docs


.. _`CLIMLAB description paper in JOSS`: http://joss.theoj.org/papers/10.21105/joss.00659
.. _`CLIMLAB recipe used on conda-forge`: https://github.com/conda-forge/climlab-feedstock
.. _`pytest`: https://docs.pytest.org/en/latest/
.. _`conda build`: https://docs.conda.io/projects/conda-build/en/latest/
.. _`Contact page`: contact.html
.. _ReStructuredText: http://docutils.sourceforge.net/docs/user/rst/quickstart.html
.. _`these instructions`: https://help.github.com/articles/fork-a-repo/
.. _`open issue on the CLIMLAB github page`: https://github.com/climlab/climlab/issues/68
.. _documentation: http://climlab.readthedocs.io
.. _`pull request`: https://help.github.com/articles/about-pull-requests/
.. _`numpy.f2py`: https://numpy.org/doc/stable/f2py/
.. _`these f2py examples`: https://numpy.org/doc/stable/f2py/f2py.getting-started.html
.. _`See here for discussion`: https://www.anaconda.com/utilizing-the-new-compilers-in-anaconda-distribution-5/
.. _`write the new test before you write the new code`: https://softwareengineering.stackexchange.com/questions/36175/what-are-the-disadvantages-of-writing-code-before-writing-unit-tests
.. _`The Climate Laboratory`: https://brian-rose.github.io/ClimateLaboratoryBook
.. _`climlab-rrtmg`: https://github.com/climlab/climlab-rrtmg
.. _`climlab-cam3-radiation`: https://github.com/climlab/climlab-cam3-radiation
.. _`climlab-emanuel-convection`: https://github.com/climlab/climlab-emanuel-convection
