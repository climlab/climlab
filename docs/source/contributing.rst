.. highlight:: rst

Contributing to CLIMLAB
================

This is an open project, and contributions of all kinds are welcome.

Here are some guidelines for how to get involved.


Usage in publications, teaching, etc.
---------------------

If you use CLIMLAB in any way for published research or theses, online teaching materials, or anything else, we would appreciate hearing about it. Our goal is to maintain a list of links and references to use cases. This is essential information for our funders (NSF) but will also be a great resource for new users looking to find out more about what you can do with CLIMLAB.

The best way to report usage is through this `open issue on the CLIMLAB github page`_. If you're not a github users you can report usage directly to Brian Rose (see `Contact page`_).

For publications, please cite the `CLIMLAB description paper in JOSS`_. The full citation is:

  Rose, (2018). CLIMLAB: a Python toolkit for interactive, process-oriented climate modeling. Journal of Open Source Software, 3(24), 659, https://doi.org/10.21105/joss.00659


Reporting bugs, issues, new feature requests, and documentation problems
-----------------------

These can all be raised as new issues at <https://github.com/brian-rose/climlab/issues>

If you are reporting a bug, try to include:

- Your operating system name and version
- The Python and CLIMLAB versions you are using
- Minimal code to reproduce the bug

If you aren't sure about any of this, please just post your issue anyway and we will assist.

Feel free to point out any inaccuracies or omissions in the documentation_ here as well.


Seeking help and support
-------------------

Although CLIMLAB is offered to the community "as-is", we are very interested in helping people actually use it for scientific purposes.

Have a question about how to do something with CLIMLAB? First, make sure you've perused the documentation_ and the issue tracker at <https://github.com/brian-rose/climlab/issues>. Also look through published examples including the repository of lecture notes at <https://github.com/brian-rose/ClimateModeling_courseware>.

Then, feel free to ask questions by opening a new issue at <https://github.com/brian-rose/climlab/issues>. This requires a free github account but is the best way to engage the community for answers. We will do our best to respond. If the functionality you're looking for doesn't yet exist, we'll probably encourage you to get involved in developing the next big feature.


Contributing bug fixes and new features
-----------------------

We are thrilled to have any and all help. You may want to browse through <https://github.com/brian-rose/climlab/issues> to see if there is any low-hanging fruit already identified.

Contributions will happen through Pull Requests on github. You will need a free github account. Here's how to get started:

1. Follow `these instructions`_ to fork the main CLIMAB repo at <https://github.com/brian-rose/climlab>, clone it on your local machine, and keep your local master branch synced with the main repo.
2. Don't make any commits on your local master branch. Instead open a feature branch for every new development task::

    git checkout -b cool_new_feature

(choose a more descriptive name for your new feature).

3. Work on your new feature, using ``git add`` to add your changes.
4. Build and Test your modified code! See `Building and Testing CLIMLAB`_ below for instructions. Make sure to add new tests for your cool new feature.

5. When your feature is complete and tested, commit your changes::

    git commit -m 'I made some cool new changes'

and push your branch to github::

    git push origin cool_new_feature

6. At this point, you go find your fork on github and create a `pull request`_. Clearly describe what you have done in the comments. We will gladly merge any pull requests that fix outstanding issues with the code or documentation. If you are adding a new feature, it is important to also add appropriate tests of the new feature to the automated test suite. If you don't know how to do this, submit your pull request anyway and we will assist.

7. After your pull request is merged, you can switch back to the master branch, rebase, and delete your feature branch. You will find your improvements are incorporated into CLIMLAB::

    git checkout master
    git fetch upstream
    git rebase upstream/master
    git branch -d cool_new_feature


Building and Testing CLIMLAB
----------------------

CLIMLAB has an extensive set of tests designed to work with `pytest`_. The test code is found in the ``climlab/tests`` directory inside the source repo.

To run the full set of tests on the currently installed version of CLIMLAB, you can always do this::

    pytest -v --pyargs climlab

All tests should report ``PASSED``.

CLIMLAB is a mix of pure Python and compiled Fortran. If you are developing new code that does not rely on the compiled components, it is useful to run tests directly from the source code directory. From the ``climlab`` root directory, do the following::

    pytest -v -m "not compiled"

which excludes the tests marked as requiring the compiled components. Again, look for all tests to report ``PASSED``. For more details see the `pytest`_ documentation.

If you are interacting with compiled components (e.g. RRTMG radiation), testing is a little more complicated. You will need to rebuild and install a new version. We use (and recommend) `conda build`_ to handle the dependencies including Fortran compiler.

To build CLIMLAB, do this from the root directory of the CLIMLAB source repo::

    conda-build conda-recipe

This will automatically install all build dependencies in a temporary new conda environment, build all the Fortran extensions, bundle everything together, install the new pacakge in a temporary test environment, and run a minimal set of tests on the package (only the tests marked as ``fast``). The whole procedure will take several minutes to run through.

Assuming the tests pass successfully, you will see a message like::

    TEST END: /Users/br546577/anaconda3/conda-bld/osx-64/climlab-0.6.5.dev0-py36_5.tar.bz2

(though obviously with different paths and version numbers)

To fully test your new build (including the tests not marked as ``fast``), you can now install it in a new test environment (with all dependencies) and run the full set of tests::

    conda create --name newtest climlab --use-local
    source activate newtest
    pytest -v --pyargs climlab

Once you're happy with this you can safely delete the test environment with::

    source deactivate
    conda remove --name newtest --all

If you encounter problems with the conda build recipe, please raise an issue at <https://github.com/brian-rose/climlab/issues>. You could also take a look at the `CLIMLAB recipe used on conda-forge`_, which might be a little more up-to-date.


Contributing improved documentation
---------------------

The documentation_ is generated with Sphinx from docstrings in the source code itself, along with a small collection of ReStructuredText_ (.rst) files. You can help improve the documentation!

- Edit doctrings and/or .rst files in ``climlab/docs/``
- Build the improved docs locally with::

    make html

from the ``climlab/docs`` directory.

- The new and improved docs should now be available locally in the ``climlab/docs/build/html`` directory. Check them out in your web browser.
- Once you are satisfied, commit changes as described above and submit a new Pull Request describing your changes.


.. _`CLIMLAB description paper in JOSS`: http://joss.theoj.org/papers/10.21105/joss.00659
.. _`CLIMLAB recipe used on conda-forge`: https://github.com/conda-forge/climlab-feedstock
.. _`pytest`: https://docs.pytest.org/en/latest/
.. _`conda build`: https://conda.io/docs/user-guide/tasks/build-packages/index.html
.. _`Contact page`: contact.html
.. _ReStructuredText: http://docutils.sourceforge.net/docs/user/rst/quickstart.html
.. _`these instructions`: https://help.github.com/articles/fork-a-repo/
.. _`open issue on the CLIMLAB github page`: https://github.com/brian-rose/climlab/issues/68
.. _documentation: http://climlab.readthedocs.io
.. _`pull request`: https://help.github.com/articles/about-pull-requests/
