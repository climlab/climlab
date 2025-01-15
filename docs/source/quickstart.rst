.. highlight:: rst

Quickstart Guide
================


Installation
------------

By far the simplest and recommended way to install ``climlab`` is using conda_
(which is the wonderful package manager that comes with `Anaconda Python`_).

You can install ``climlab`` and all its dependencies with::

    conda install -c conda-forge climlab

Or (recommended) add ``conda-forge`` to your conda channels with::

    conda config --add channels conda-forge

and then simply do::

    conda install climlab

Binaries are available for OSX, Linux, and Windows.


Single-column Radiative-Convective model
----------------------------------------

Here is a quick example of setting up a single-column
Radiative-Convective model with fixed relative humidity, using the
RRTMG radiation scheme:

    .. code-block:: python

        import climlab
        alb = 0.25
        #  State variables (Air and surface temperature)
        mystate = climlab.column_state(num_lev=30)
        #  Fixed relative humidity
        h2o = climlab.radiation.ManabeWaterVapor(name='Water Vapor',
                                                 state=mystate)
        #  Couple water vapor to radiation
        rad = climlab.radiation.RRTMG(name='Radiation',
                                      state=mystate, 
                                      specific_humidity=h2o.q, 
                                      albedo=alb)
        #  Convective adjustment
        conv = climlab.convection.ConvectiveAdjustment(name='Convective Adjustment',
                                                       state=mystate, 
                                                       adj_lapse_rate=6.5)
        #  Couple everything together into a parent process
        rcm = climlab.couple([rad,conv,h2o], name='Radiative Convective Model')
        #  Inspect the model
        print(rcm)
        #  Run the model
        rcm.integrate_years(1)
        #  Check for energy balance
        print(rcm.ASR - rcm.OLR)

.. _conda: https://conda.io/docs/
.. _`Anaconda Python`: https://www.anaconda.com/distribution/
