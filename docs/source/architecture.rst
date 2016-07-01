.. highlight:: rst

Architecture
============

The backbone of the climlab architecture are the :class:`~climlab.process.process.Process`
and :class:`~climlab.process.time_dependent_process.TimeDependentProcess` classes.
All model components in `climlab` are instances of :class:`~climlab.process.process.Process`.
Conceptually, a :class:`~climlab.process.process.Process` object represents any
physical mechanism that can be described in terms of one or more **state variables** and
processes that modify those variables.

As all relevant procedures and events that can be modelled with `climlab`
are expressed in `Processes`, they build the basic structure of the package.

For example, if you want to model the incoming solar radiation on Earth,
`climlab` implements it as a `Process`, namely in the `Diagnostic Process` :class:`~climlab.radiation.insolation._Insolation`
(or one of its specific daughter classes).

Another example: the emitted energy of a surface can be computed through  the
:class:`~climlab.radiation.Boltzmann.Boltzmann` class which is also a climlab
`Process` and implements the Stefan Boltzmann Law for a grey body.
Like that, all events and procedures that `climlab` can model are organized in `Processes`.


.. note::

	The implementation of a whole model, for example an Energy Balance Model (:class:`~climlab.model.ebm.EBM`), is also an instance of the :class:`~climlab.process.process.Process` class in `climlab`.

	For more information about models, see the climlab :ref:`models` chapter.

A :class:`~climlab.process.process.Process` object contains
a `subprocess` dictionary, which itself can contain an arbitraily complex
collection of other :class:`~climlab.process.process.Process` objects.

A `Process` that represents a whole model
will typically have some `subprocesses` which represent
specific physical components of the model,
for example the albedo or the insolation component.
More details about subprocesses can be found below.

The `state` variables of a `Process` are always defined on a **Domain**
which itself is based on **Axes** or a single **Axis**. The following section will give a basic introduction about their role in the package, their dependencies and their implementation.


.. _process_architecture:

*******
Process
*******

A Process is an instance of the class :class:`~climlab.process.process.Process`. Most processes are time-dependent and therefore an instance of the daughter class :class:`~climlab.process.time_dependent_process.TimeDependentProcess`.

Basic Dictionaries
##################

A `climlab.Process` object has several iterable dictionaries (:py:class:`dict`) of named, gridded variables [#]_:

	- ``process.state``
		contains the `process`' state variables, which are usually time-dependent and which are major quantities that identify the condition and status of the `process`. This can be the (surface) temperature of a model for instance.

	- ``process.input``
	    	contains boundary conditions and other gridded quantities independent of the `process`. This dictionary is often set by a parent `process`.

	- ``process.param``
		contains parameter of the `Process` or model. Basically, this is the same as ``process.input`` but with scalar entries.

	- ``process.tendencies``
		is an iterable dictionary of time tendencies :math:`(d/dt)` for each state variable defined in ``process.state``.

		.. note::

			A non TimeDependentProcess (but instance of :class:`~climlab.process.process.Process`) does not have this dictionary.

	- ``process.diagnostics``
	    	contains any quantity derived from the current state. In an Energy Balance Model this dictionary can have entries like ``'ASR'``, ``'OLR'``, ``'icelat'``, ``'net_radiation'``, ``'albedo'`` or ``'insolation'``.

	- ``process.subprocess``
		holds subprocesses of the `process`. More about subprocesses is described below.

The `process` is fully described by contents of `state`, `input` and `param`
dictionaries. `tendencies` and `diagnostics` are always computable from the current
state.

.. [#] In the following the small written `process` refers to an instance of the :class:`~climlab.process.process.Process` class.

Subprocesses
############

Subprocesses are representing and modeling certain components of the parent process. A model consists of many subprocesses which are usually defined on the same state variables, domains and axes as the parent process, at least partially.

	:Example: The subprocess tree of an EBM may look like this:

		.. code-block:: python

			model_EBM		#<head process>
			   diffusion		#<subprocess>
			   LW			#<subprocess>
			   albedo		#<subprocess>
			      iceline		#<sub-subprocess>
			      cold_albedo	#<sub-subprocess>
			      warm_albedo	#<sub-subprocess>
			   insolation		#<subprocess>

It can be seen that subprocesses can have subprocesses themselves, like ``albedo`` in this case.

A ``subprocess`` is similar to its ``parent process`` an instance of the :class:`~climlab.process.process.Process` class. That means a ``subprocess`` has dictionaries and attributes with the same names as its ``parent process``. Not necessary all will be the same or have the same entries, but a ``subprocess`` has at least the basic dictionaries and attributes created during initialization of the :class:`~climlab.process.process.Process` instance.

Every `subprocess` should work independently of its `parent process` given
appropriate `input`.

	:Example: Investigating an individual `process` (possibly with its own `subprocesses`) isolated from its parent can be done through:

		.. code-block:: python

			newproc = climlab.process_like(procname.subprocess['subprocname'])
			newproc.compute()

		Thereby anything in the `input` dictionary of ``'subprocname'`` will remain fixed.




Process Integration over time
#############################

A :class:`~climlab.process.time_dependent_process.TimeDependentProcess` can be integrated over time to see how the state variables and other diagnostic variables vary in time.

Time Dependency of a State Variable
-----------------------------------

For a state variable :math:`S` which is dependendet on processes :math:`P_A`, :math:`P_B`, ... the time dependency can be written as

.. math::

	\frac{dS}{dt} = \underbrace{P_A(S)}_{S \textrm{ tendency by }P_A} + \underbrace{P_B(S)}_{S \textrm{ tendency by } P_B} + \ ...

When the state variable :math:`S` is discretized over time like

.. math::

	\frac{dS}{dt} = \frac{\Delta S}{\Delta t} = \frac{S(t_1) - S(t_0)}{t_1 - t_0} = \frac{S_1 - S_0}{\Delta t} ~,

the state tendency can be calculated through

.. math::

	\Delta S = \big[ P_A(S) + P_B(S) + \ ... \big] \Delta t

and the new state of :math:`S` after one timestep :math:`\Delta t` is then:

.. math::

	S_1 = S_0 + \big[ \underbrace{P_A(S)}_{S \textrm{ tendency by }P_A} + \underbrace{P_B(S)}_{S \textrm{ tendency by }P_B} + \ ... \ \big] \Delta t  ~.


Therefore, the new state of :math:`S` is calculated by multiplying the process tendencies of :math:`S` with the timestep and adding them up to the previous state of :math:`S`.

Time Dependency of an Energy Budget
-----------------------------------

The time dependency of an EBM energy budget is very similar to the above noted equations, just differing in a heat capacity factor :math:`C`. The state variable is temperature :math:`T` in this case, which is altered by subprocesses :math:`SP_A`, :math:`SP_B`, ...

.. math::

	\frac{dE}{dt} = C \frac{dT}{dt} = \underbrace{SP_A(T)}_{\textrm{heating-rate of }SP_A} + \underbrace{SP_B(T)}_{\textrm{ heating-rate of }SP_B} + \ ...  			\\
	\Leftrightarrow   \frac{dT}{dt} = \underbrace{\frac{SP_A(T)}{C}}_{T \textrm{ tendency by }SP_A} + \underbrace{\frac{SP_B(T)}{C}}_{T \textrm{ tendency by }SP_B} + \ ...



Therefore, the new state of :math:`T` after one timestep :math:`\Delta t` can be written as:

.. math::

	T_1 = \underbrace{T_0 + \underbrace{ \left[ \frac{SP_A(T)}{C} + \frac{SP_B(T)}{C} + \ ... \right]}_{\textrm{compute()}}  \Delta t }_{\textrm{step\_forward()}}


The integration procedure is implemented in multiple nested function calls. The top functions for model integration are explained here, for details about computation of subprocess tendencies see `Classification of Subprocess Types`_ below.

- :class:`~climlab.process.time_dependent_process.TimeDependentProcess.compute()` is a method that computes tendencies :math:`d/dt` for all state variables
    - it returns a dictionary of tendencies for all state variables

	Temperature tendencies are :math:`\frac{SP_A(T)}{C}`, :math:`\frac{SP_B(T)}{C}`, ... in this case, which are summed up like:

	.. math::

		\textrm{tendencies}(T) = \frac{SP_A(T)}{C} + \frac{SP_B(T)}{C} + ...

    - the keys for this dictionary are the same as keys of state dictionary

	As temperature :math:`T` is the only state variable in this energy budget, the tendencies dictionary also just has the one key, representing the state variable :math:`T`.

    - the tendency dictionary holds the total tendencies for each state including all subprocesses

	In case subprocess :math:`SP_A` itself has subprocesses, their :math:`T` tendencies get included in tendency computation by :class:`~climlab.process.time_dependent_process.TimeDependentProcess.compute()`.

    - the method only computes :math:`d/dt` but **does not apply changes** (which is done by :class:`~climlab.process.time_dependent_process.TimeDependentProcess.step_forward()`)
    - therefore, the method is relatively independent of the numerical scheme
    - method **will update** variables in ``proc.diagnostic`` dictionary. Therefore, it will also **gather all diagnostics** from the `subprocesses`

- :class:`~climlab.process.time_dependent_process.TimeDependentProcess.step_forward()` updates the state variables
    - it calls :class:`~climlab.process.time_dependent_process.TimeDependentProcess.compute()` to get current tendencies
    - the method multiplies state tendencies with the timestep and adds them up to the state variables

- :class:`~climlab.process.time_dependent_process.TimeDependentProcess.integrate_years()` etc will automate time-stepping by calling the :class:`~climlab.process.time_dependent_process.TimeDependentProcess.step_forward` method multiple times. It also does the computation of time-average diagnostics.

- :class:`~climlab.process.time_dependent_process.TimeDependentProcess.integrate_converge()` calls :class:`~climlab.process.time_dependent_process.TimeDependentProcess.integrate_years()` as long as the state variables keep changing over time.

:Example: Integration of a `climlab` EBM model over time can look like this:

	.. code-block:: python

		import climlab
		model = climlab.EBM()

		# integrate the model for one year
		model.integrate_years(1)


Classification of Subprocess Types
----------------------------------

Processes can be classified in types: `explicit`, `implicit`, `diagnostic` and `adjustment`.
This makes sense as subprocesses may have different impact on state variable tendencies (`diagnostic` processes don't have a direct influence for instance) or the way their tendencies are computed differ (`explixit` and `implicit`).

Therefore, the :class:`~climlab.process.time_dependent_process.TimeDependentProcess.compute()` method handles them seperately as well as in specific order. It calls private :func:`_compute()` methods that are specified in daugther classes of :class:`~climlab.process.process.Process` namely :class:`~climlab.process.diagnostic.DiagnosticProcess`,
:class:`~climlab.process.energy_budget.EnergyBudget` (which are explicit processes) or
:class:`~climlab.process.implicit.ImplicitProcess`.

The description of :class:`~climlab.process.time_dependent_process.TimeDependentProcess.compute()` reveals the details how the different process types are handeled:

..

        The function first computes all diagnostic processes. They don't produce
	any tendencies directly but they may effect the other processes (such as
	change in solar distribution). Subsequently, all tendencies and diagnostics
	for all explicit processes are computed.

        Tendencies due to implicit and adjustment processes need to be
        calculated from a state that is already adjusted after explicit
        alteration. For that reason the explicit tendencies are applied to the states
        temporarily. Now all tendencies from implicit processes are calculated
        by matrix inversions and similar to the explicit tendencies, the implicit ones
	are applied to the states temporarily. Subsequently, all instantaneous adjustments
        are computed.

        Then the changes that were made to the states from explicit and implicit
        processes are removed again as this
	:class:`~climlab.process.time_dependent_process.TimeDependentProcess.compute()`
 	function is
        supposed to calculate only tendencies and not apply them to the states.

        Finally, all calculated tendencies from all processes are collected
        for each state, summed up and stored in the dictionary
        ``self.tendencies``, which is an attribute of the time-dependent-process
        object, for which the
	:class:`~climlab.process.time_dependent_process.TimeDependentProcess.compute()`
	method has been called.



******
Domain
******

A `Domain` defines an area or spatial base for a climlab :class:`~climlab.process.process.Process` object. It consists of axes which
are :class:`~climlab.domain.axis.Axis` objects that define the dimensions of the `Domain`.

In a `Domain` the heat capacity of grid points, bounds or cells/boxes is specified.

There are daughter classes :class:`~climlab.domain.domain.Atmosphere` and :class:`~climlab.domain.domain.Ocean` of the private :class:`~climlab.domain.domain._Domain` class implemented which themselves have daughter classes :class:`~climlab.domain.domain.SlabAtmosphere` and :class:`~climlab.domain.domain.SlabOcean`.

Every :class:`~climlab.process.process.Process` needs to be defined on a `Domain`. If none is given during initialization but latitude ``lat`` is specified, a default `Domain` is created.

Several methods are implemented that create `Domains` with special specifications. These are

	- :func:`~climlab.domain.domain.single_column`

	- :func:`~climlab.domain.domain.zonal_mean_column`

	- :func:`~climlab.domain.domain.box_model_domain`

****
Axis
****

An :class:`~climlab.domain.axis.Axis` is an object where information of a :class:`~climlab.domain.domain._Domain`'s spacial dimension are specified.

These include the `type` of the axis, the `number of points`, location of `points` and `bounds` on the spatial dimension, magnitude of bounds differences `delta` as well as their `unit`.

The `axes` of a :class:`~climlab.domain.domain._Domain` are stored in the dictionary axes, so they can be accessed through ``dom.axes`` if ``dom`` is an instance of :class:`~climlab.domain.domain._Domain`.

*************
Accessibility
*************

For convenience with interactive work, each subprocess ``'name'`` should be accessible
as ``proc.subprocess.name`` as well as the regular way through the subprocess dictionary ``proc.subprocess['name']``. Note that ``proc`` is an instance of the :class:`~climlab.process.process.Process` class here.


:Example:

	.. code-block:: python

		import climlab
		model = climlab.EBM()

		# quick access
		longwave_subp = model.subprocess.LW

		# regular path
		longwave_subp = model.subprocess['LW']


`climlab` will remain (as much as possible) agnostic about the data formats. Variables within the dictionaries will behave as :py:class:`numpy.ndarray` objects.

Grid information and other domain details are accessible as attributes of each process.
These attributes are ``lat``, ``lat_bounds``, ``lon``, ``lon_bounds``, ``lev``, ``lev_bounds``, ``depth`` and ``depth_bounds``.

:Example: the latitude points of a `process` object that is describing an EBM model

	.. code-block:: python

		import climlab
		model = climlab.EBM()

		# quick access
		lat_points = model.lat

		# regular path
		lat_points = model.domains['Ts'].axes['lat'].points


Shortcuts like ``proc.lat`` will work where these are unambiguous, which means there is only a single axis of that type in the process.

Many variables will be accessible as process attributes ``proc.name``. This restricts to unique field names in the above dictionaries.

.. warning::

	There may be other dictionaries that do have name conflicts: e.g. dictionary of tendencies ``proc.tendencies``, with same keys as ``proc.state``.

	These will **not be accessible** as ``proc.name``, but **will be accessible** as ``proc.dict_name.name`` (as well as regular dictionary interface ``proc.dict_name['name']``).
