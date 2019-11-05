.. highlight:: rst

Quickstart Guide
================


Installation
--------------

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
RRTMG radiation scheme::

            import climlab
            alb = 0.25
            #  State variables (Air and surface temperature)
            state = climlab.column_state(num_lev=30)
            #  Parent model process
            rcm = climlab.TimeDependentProcess(state=state)
            #  Fixed relative humidity
            h2o = climlab.radiation.ManabeWaterVapor(state=state)
            #  Couple water vapor to radiation
            rad = climlab.radiation.RRTMG(state=state, specific_humidity=h2o.q, albedo=alb)
            #  Convective adjustment
            conv = climlab.convection.ConvectiveAdjustment(state=state, adj_lapse_rate=6.5)
            #  Couple everything together
            rcm.add_subprocess('Radiation', rad)
            rcm.add_subprocess('WaterVapor', h2o)
            rcm.add_subprocess('Convection', conv)
            #  Run the model
            rcm.integrate_years(1)
            #  Check for energy balance
            print rcm.ASR - rcm.OLR

.. _conda: https://conda.io/docs/
.. _`Anaconda Python`: https://www.anaconda.com/distribution/
