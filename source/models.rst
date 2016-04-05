.. highlight:: rst

.. _models:

Models
======

As indicated in the :ref:`Introduction` `climlab` can implement different types of models out of the box.
Here we focus on Energy Balance Models which are refered to as EBMs.

Energy Balance Model
--------------------

Currently there are three "standard" Energy Balance Models implemented in the `climlab` code. 
These are :class:`~climlab.model.ebm.EBM`, :class:`~climlab.model.ebm.EBM_seasonal` and :class:`~climlab.model.ebm.EBM_annual`, which are explained below.

Let's first give an overview about different (sub)processes that are implemented:

EBM Subprocesses
^^^^^^^^^^^^^^^^

Insolation
::::::::::

- :class:`~climlab.radiation.insolation.FixedInsolation`
	defines a constant solar factor for all spatial points of the domain. 

	.. math::

		S(lat) = S_{\textrm{input}}

- :class:`~climlab.radiation.insolation.P2Insolation`
	characterizes a parabolic solar distribution over the domain's latitude on basis of the second order Legendre Polynomial :math:`P_2`:

	.. math::

		S(lat) = \frac{S_0}{4} \big[1+ s_2 P_2(\sin lat) \big]

- :class:`~climlab.radiation.insolation.DailyInsolation` 
	computes the daily solar insolation for each latitude off the domain on the basis of orbital parameters and astronomical formulas.
- :class:`~climlab.radiation.insolation.AnnualMeanInsolation`
	computes a latitudewise yearly mean for solar insolation on the basis of orbital parameters and astronomical formulas.


Albedo
::::::

- :class:`~climlab.surface.albedo.ConstantAlbedo`
	defines constant albedo values at all spatial points of the domain:

	.. math::

		\alpha(lat) = a_0 

- :class:`~climlab.surface.albedo.P2Albedo`
	initializes parabolic distributed albedo values across the domain on basis of the second order Legendre Polynomial :math:`P_2`:

	.. math::

		\alpha(lat) = a_0 + a_2 P_2(\sin lat)

- :class:`~climlab.surface.albedo.Iceline`
	determines which part of the domain is covered with ice according to a given freezing temperature.

- :class:`~climlab.surface.albedo.StepFunctionAlbedo`
	implements an albedo step function in dependence of the surface temperature through using instances of the above described albedo classes as subprocesses. 

Outgoing Longwave Radiation
:::::::::::::::::::::::::::

- :class:`~climlab.radiation.AplusBT.AplusBT`
	calculates the Outgoing Longwave Radiation (:math:`OLR`) in form of a linear dependence of surface temperature :math:`T` like 
	
	.. math::

		OLR = A+B \cdot T

- :class:`~climlab.radiation.AplusBT.AplusBT_CO2`
	calculates OLR same as :class:`~climlab.radiation.AplusBT.AplusBT` but uses parameters :math:`A` and :math:`B` dependent of the atmospheric CO2 concentration :math:`c`.

	.. math::

		OLR = A(c)+B(c) \cdot T


- :class:`~climlab.radiation.Boltzmann.Boltzmann`
	calculates OLR after the Stefan-Boltzmann law for a grey body like

	.. math::

		OLR = \sigma \varepsilon T^4


Energy Transport
::::::::::::::::

These classes calculate the transport of energy :math:`H(\varphi)` across the latitude :math:`\varphi` in an Energy Budget noted as:

.. math::

	C(\varphi) \frac{dT(\varphi)}{dt} = R\downarrow (\varphi) - R\uparrow (\varphi) + H(\varphi) 

- :class:`~climlab.dynamics.diffusion.MeridionalDiffusion`
	calculates the energy transport in a diffusion like process along the temperature gradient:

	.. math::
	
		H(\varphi) = \frac{D}{\cos \varphi}\frac{\partial}{\partial \varphi} \left( \cos\varphi \frac{\partial T(\varphi)}{\partial \varphi} \right)
	
- :class:`~climlab.dynamics.budyko_transport.BudykoTransport`
	calculates the energy transport for each latitude :math:`\varphi` in relation to the global mean temperature :math:`\bar{T}`:

	.. math::
	
		H(\varphi) = - b [T(\varphi) - \bar{T}]
	


EBM templates
^^^^^^^^^^^^^

The preconfigured Energy Balance Models `EBM`_, `EBM_seasonal`_ and `EBM_annual`_ use the described suprocesses above: 

EBM
:::

The :class:`~climlab.model.ebm.EBM` class sets up a typical Energy Balance Model with following subprocesses:

    * Outgoing Longwave Radiation (OLR) parameterization through 
      :class:`~climlab.radiation.AplusBT.AplusBT`
    * solar insolation paramterization through 
      :class:`~climlab.radiation.insolation.P2Insolation`
    * albedo parameterization in dependence of temperature through
      :class:`~climlab.surface.albedo.StepFunctionAlbedo`
    * energy diffusion through 
      :class:`~climlab.dynamics.diffusion.MeridionalDiffusion`


EBM_seasonal
::::::::::::

The :class:`~climlab.model.ebm.EBM_seasonal` class implements Energy Balance Models with realistic daily insolation.
It uses following subprocesses:

    * Outgoing Longwave Radiation (OLR) parameterization through 
      :class:`~climlab.radiation.AplusBT.AplusBT`
    * solar insolation paramterization through 
      :class:`~climlab.radiation.insolation.DailyInsolation`
    * albedo parameterization in dependence of temperature through
      :class:`~climlab.surface.albedo.StepFunctionAlbedo`
    * energy diffusion through 
      :class:`~climlab.dynamics.diffusion.MeridionalDiffusion`
        

EBM_annual
::::::::::

The :class:`~climlab.model.ebm.EBM_annual` class that implements Energy Balance Models with annual mean insolation.
It uses following subprocesses:

    * Outgoing Longwave Radiation (OLR) parameterization through 
      :class:`~climlab.radiation.AplusBT.AplusBT`
    * solar insolation paramterization through 
      :class:`~climlab.radiation.insolation.AnnualMeanInsolation`
    * albedo parameterization in dependence of temperature through
      :class:`~climlab.surface.albedo.StepFunctionAlbedo`
    * energy diffusion through 
      :class:`~climlab.dynamics.diffusion.MeridionalDiffusion`

.. note::

	For information how to set up individual models or modify instances of the classes above, see the :ref:`Tutorial` chapter.


Other Models
------------

As noted in the :ref:`Introduction` more model types are implemented in climlab but not covered in the documentation yet.

