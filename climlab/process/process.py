
#==============================================================================
# Principles of the new `climlab` API design:
#
#     * `climlab.Process` object has several iterable dictionaries of named,
#       gridded variables:
#
#         * `process.state`
#
#             * state variables, usually time-dependent
#
#         - `process.input`
#             - boundary conditions and other gridded quantities independent of the
#             `process`
#             - often set by a parent `process`
#         - `process.param`  (which are basically just scalar `input`)
#         - `process.tendencies`
#             - iterable `dict` of time-tendencies (d/dt) for each state variable
#         - `process.diagnostics`
#             - any quantity derived from current state
# - The `process` is fully described by contents of `state`, `input` and `param`
# dictionaries. `tendencies` and `diagnostics` are always computable from current
# state.
# - `climlab` will remain (as much as possible) agnostic about the data formats
#     - Variables within the dictionaries will behave as `numpy.ndarray` objects
#     - Grid information and other domain details accessible as attributes
#     of each variable
#         - e.g. Tatm.lat
#         - Shortcuts like `process.lat` will work where these are unambiguous
# - Many variables will be accessible as process attributes `process.name`
#     - this restricts to unique field names in the above dictionaries
# - There may be other dictionaries that do have name conflicts
#     - e.g. dictionary of tendencies, with same keys as `process.state`
#     - These will *not* be accessible as `process.name`
#     - but *will* be accessible as `process.dict_name.name`
#     (as well as regular dict interface)
# - There will be a dictionary of named subprocesses `process.subprocess`
# - Each item in subprocess dict will itself be a `climlab.Process` object
# - For convenience with interactive work, each subprocess should be accessible
# as `process.subprocess.name` as well as `process.subprocess['name']`
# - `process.compute()` is a method that computes tendencies (d/dt)
#     - returns a dictionary of tendencies for all state variables
#     - keys for this dictionary are same as keys of state dictionary
#     - tendency dictionary is the total tendency including all subprocesses
#     - method only computes d/dt, does not apply changes
#     - thus method is relatively independent of numerical scheme
#         - may need to make exception for implicit scheme?
#     - method *will* update variables in `process.diagnostic`
#         - will also *gather all diagnostics* from `subprocesses`
# - `process.step_forward()` updates the state variables
#     - calls `process.compute()` to get current tendencies
#     - implements a particular time-stepping scheme
#     - user interface is agnostic about numerical scheme
# - `process.integrate_years()` etc will automate time-stepping
#     - also computation of time-average diagnostics.
# - Every `subprocess` should work independently of its parent `process` given
# appropriate `input`.
#     - investigating an individual `process` (possibly with its own
#     `subprocesses`) isolated from its parent needs to be as simple as doing:
#         - `newproc = climlab.process_like(procname.subprocess['subprocname'])`
#
#         - `newproc.compute()`
#         - anything in the `input` dictionary of `subprocname` will remain fixed
#==============================================================================

from __future__ import division
import time, copy
import numpy as np
from climlab.domain.field import Field
from climlab.domain.domain import _Domain, zonal_mean_surface
from climlab.utils import walk, attr_dict


def _make_dict(arg, argtype):
    if arg is None:
        return {}
    elif isinstance(arg, dict):
        return arg
    elif isinstance(arg, argtype):
        return {'default': arg}
    else:
        raise ValueError('Problem with input type')


class Process(object):
    """A generic parent class for all climlab process objects.
    Every process object has a set of state variables on a spatial grid.

    For more general information about `Processes` and their role in climlab,
    see :ref:`process_architecture` section climlab-architecture.

    **Initialization parameters** \n

    An instance of ``Process`` is initialized with the following
    arguments *(for detailed information see Object attributes below)*:

    :param Field state: spatial state variable for the process.
                        Set to ``None`` if not specified.
    :param domains:     domain(s) for the process
    :type domains:      :class:`~climlab.domain.domain._Domain` or dict of
                        :class:`~climlab.domain.domain._Domain`
    :param subprocess:  subprocess(es) of the process
    :type subprocess:   :class:`~climlab.process.process.Process` or dict of
                        :class:`~climlab.process.process.Process`
    :param array lat:   latitudinal points (optional)
    :param lev:         altitudinal points (optional)
    :param int num_lat: number of latitudional points (optional)
    :param int num_levels:
                        number of altitudinal points (optional)
    :param dict input:  collection of input quantities

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.process.process.Process`
    following object attributes are generated during initialization:

    :ivar dict domains:     dictionary of process :class:`~climlab.domain.domain._Domain`
    :ivar dict state:       dictionary of process states
                            (of type :class:`~climlab.domain.field.Field`)
    :ivar dict param:       dictionary of model parameters which are given
                            through ``**kwargs``
    :ivar dict diagnostics: a dictionary with all diagnostic variables
    :ivar dict _input_vars: collection of input quantities like boundary conditions
                            and other gridded quantities
    :ivar str creation_date:
                            date and time when process was created
    :ivar subprocess:       dictionary of suprocesses of the process
    :vartype subprocess:    dict of :class:`~climlab.process.process.Process`

    """

    def __str__(self):
        str1 = 'climlab Process of type {0}. \n'.format(type(self))
        str1 += 'State variables and domain shapes: \n'
        for varname in self.state.keys():
            str1 += '  {0}: {1} \n'.format(varname, self.domains[varname].shape)
        str1 += 'The subprocess tree: \n'
        str1 += walk.process_tree(self)
        return str1

    def __init__(self, state=None, domains=None, subprocess=None,
                 lat=None, lev=None, num_lat=None, num_levels=None,
                 input=None, **kwargs):
        # dictionary of domains. Keys are the domain names
        self.domains = _make_dict(domains, _Domain)
        #  If lat is given, create a simple domains
        if lat is not None:
            sfc = zonal_mean_surface()
            self.domains.update({'default': sfc})
        # dictionary of state variables (all of type Field)
        self.state = attr_dict.AttrDict()
        states = _make_dict(state, Field)
        for name, value in states.iteritems():
            self.set_state(name, value)
        #convert int dtype to float
            if np.issubdtype(self.state[name].dtype, 'int'):
                value = self.state[name].astype(float)
                self.set_state(name, value)
        # dictionary of model parameters
        self.param = kwargs
        # dictionary of diagnostic quantities
        #self.diagnostics = attr_dict.AttrDict()
        #self._diag_vars = frozenset()
        self._diag_vars = []
        # dictionary of input quantities
        #self.input = _make_dict(input, Field)
        if input is None:
            #self._input_vars = frozenset()
            self._input_vars = []
        else:
            self.add_input(input.keys())
            for name, var in input:
                self.__dict__[name] = var
        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
        # subprocess is a dictionary of any sub-processes
        self.subprocess = attr_dict.AttrDict()
        if subprocess is not None:
            self.add_subprocesses(subprocess)
        #if subprocess is None:
        #    #self.subprocess = {}
        #    # a dictionary whose items can be accessed as attributes
        #    self.subprocess = attr_dict.AttrDict()
        #else:
        #    self.add_subprocesses(subprocess)

    def add_subprocesses(self, procdict):
        """Adds a dictionary of subproceses to this process.

        Calls :func:`add_subprocess` for every process given in the
        input-dictionary. It can also pass a single process, which will
        be given the name *default*.

        :param procdict:    a dictionary with process names as keys
        :type procdict:     dict

        """
        if isinstance(procdict, Process):
            self.add_subprocess('default', procdict)
        else:
            for name, proc in procdict.iteritems():
                self.add_subprocess(name, proc)

    def add_subprocess(self, name, proc):
        """Adds a single subprocess to this process.

        :param string name:     name of the subprocess
        :param proc:            a Process object
        :type proc:             :class:`~climlab.process.process.Process`
        :raises: :exc:`ValueError`
                                if ``proc`` is not a process

        :Example:

            Replacing an albedo subprocess through adding a subprocess with
            same name::

                >>> from climlab.model.ebm import EBM_seasonal
                >>> from climlab.surface.albedo import StepFunctionAlbedo

                >>> # creating EBM model
                >>> ebm_s = EBM_seasonal()

                >>> print ebm_s

            .. code-block:: none
                :emphasize-lines: 8

                climlab Process of type <class 'climlab.model.ebm.EBM_seasonal'>.
                State variables and domain shapes:
                  Ts: (90, 1)
                The subprocess tree:
                top: <class 'climlab.model.ebm.EBM_seasonal'>
                   diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
                   LW: <class 'climlab.radiation.AplusBT.AplusBT'>
                   albedo: <class 'climlab.surface.albedo.P2Albedo'>
                   insolation: <class 'climlab.radiation.insolation.DailyInsolation'>

            ::

                >>> #  creating and adding albedo feedback subprocess
                >>> step_albedo = StepFunctionAlbedo(state=ebm_s.state, **ebm_s.param)
                >>> ebm_s.add_subprocess('albedo', step_albedo)
                >>>
                >>> print ebm_s

            .. code-block:: none
                :emphasize-lines: 8

                climlab Process of type <class 'climlab.model.ebm.EBM_seasonal'>.
                State variables and domain shapes:
                  Ts: (90, 1)
                The subprocess tree:
                top: <class 'climlab.model.ebm.EBM_seasonal'>
                   diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
                   LW: <class 'climlab.radiation.AplusBT.AplusBT'>
                   albedo: <class 'climlab.surface.albedo.StepFunctionAlbedo'>
                      iceline: <class 'climlab.surface.albedo.Iceline'>
                      cold_albedo: <class 'climlab.surface.albedo.ConstantAlbedo'>
                      warm_albedo: <class 'climlab.surface.albedo.P2Albedo'>
                   insolation: <class 'climlab.radiation.insolation.DailyInsolation'>

        """
        if isinstance(proc, Process):
            self.subprocess.update({name: proc})
            self.has_process_type_list = False
            # Add subprocess diagnostics to parent
            #  (if there are no name conflicts)
            for diagname, value in proc.diagnostics.iteritems():
                if not (diagname in self.diagnostics or hasattr(self, diagname)):
                    self.add_diagnostic(diagname, value)
        else:
            raise ValueError('subprocess must be Process object')

    def remove_subprocess(self, name, verbose=True):
        """Removes a single subprocess from this process.

        :param string name:     name of the subprocess
        :param bool verbose:    information whether warning message
                                should be printed [default: True]

        :Example:

            Remove albedo subprocess from energy balance model::

                >>> import climlab
                >>> model = climlab.EBM()

                >>> print model
                climlab Process of type <class 'climlab.model.ebm.EBM'>.
                State variables and domain shapes:
                  Ts: (90, 1)
                The subprocess tree:
                top: <class 'climlab.model.ebm.EBM'>
                   diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
                   LW: <class 'climlab.radiation.AplusBT.AplusBT'>
                   albedo: <class 'climlab.surface.albedo.StepFunctionAlbedo'>
                      iceline: <class 'climlab.surface.albedo.Iceline'>
                      cold_albedo: <class 'climlab.surface.albedo.ConstantAlbedo'>
                      warm_albedo: <class 'climlab.surface.albedo.P2Albedo'>
                   insolation: <class 'climlab.radiation.insolation.P2Insolation'>

                >>> model.remove_subprocess('albedo')

                >>> print model
                climlab Process of type <class 'climlab.model.ebm.EBM'>.
                State variables and domain shapes:
                  Ts: (90, 1)
                The subprocess tree:
                top: <class 'climlab.model.ebm.EBM'>
                   diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
                   LW: <class 'climlab.radiation.AplusBT.AplusBT'>
                   insolation: <class 'climlab.radiation.insolation.P2Insolation'>

        """
        try:
            self.subprocess.pop(name)
        except KeyError:
            if verbose:
                print 'WARNING: {} not found in subprocess dictionary.'.format(name)
        self.has_process_type_list = False

    def set_state(self, name, value):
        """Sets the variable ``name`` to a new state ``value``.

        :param string name:     name of the state
        :param value:           state variable
        :type value:            :class:`~climlab.domain.field.Field` or *array*
        :raises: :exc:`ValueError`
                                if state variable ``value`` is not having a domain.
        :raises: :exc:`ValueError`
                                if shape mismatch between existing domain and
                                new state variable.

        :Example:

            Resetting the surface temperature of an EBM to
            :math:`-5 ^{\circ} \\textrm{C}` on all latitues::

                >>> import climlab
                >>> from climlab import Field
                >>> import numpy as np

                >>> # setup model
                >>> model = climlab.EBM(num_lat=36)

                >>> # create new temperature distribution
                >>> initial = -5 * ones(size(model.lat))
                >>> model.set_state('Ts', Field(initial, domain=model.domains['Ts']))

                >>> np.squeeze(model.Ts)
                Field([-5., -5., -5., -5., -5., -5., -5., -5., -5., -5., -5., -5., -5.,
                       -5., -5., -5., -5., -5., -5., -5., -5., -5., -5., -5., -5., -5.,
                       -5., -5., -5., -5., -5., -5., -5., -5., -5., -5.])

        """
        if isinstance(value, Field):
            # populate domains dictionary with domains from state variables
            self.domains.update({name: value.domain})
        else:
            try:
                thisdom = self.state[name].domain
                domshape = thisdom.shape
            except:
                raise ValueError('State variable needs a domain.')
            value = np.atleast_1d(value)
            if value.shape == domshape:
                value = Field(value, domain=thisdom)
            else:
                raise ValueError('Shape mismatch between existing domain and new state variable.')
        # set the state dictionary
        self.state[name] = value
        for name, value in self.state.iteritems():
        #convert int dtype to float
            if np.issubdtype(self.state[name].dtype, 'int'):
                value = self.state[name].astype(float)
                self.state[name]=value
        setattr(self, name, value)

    def _guess_state_domains(self):
        for name, value in self.state.iteritems():
            for domname, dom in self.domains.iteritems():
                if value.shape == dom.shape:
                    # same shape, assume it's the right domain
                    self.state_domain[name] = dom

    def _add_field(self, field_type, name, value):
        """Adds a new field to a specified dictionary. The field is also added
        as a process attribute. field_type can be 'input', 'diagnostics' """
        try:
            self.__getattribute__(field_type).update({name: value})
        except:
            raise ValueError('Problem with field_type %s'  %field_type)
        #  Note that if process has attribute name, this will trigger The
        # setter method for that attribute
        self.__setattr__(name, value)

    def add_diagnostic(self, name, value=None):
        """Create a new diagnostic variable called ``name`` for this process
        and initialize it with the given ``value``.

        Quantity is accessible in two ways:

            * as a process attribute, i.e. ``proc.name``
            * as a member of the diagnostics dictionary,
              i.e. ``proc.diagnostics['name']``

        Use attribute method to set values, e.g.
        ```proc.name = value ```

        :param str name:        name of diagnostic quantity to be initialized
        :param array value:     initial value for quantity [default: None]

        :Example:

            Add a diagnostic CO2 variable to an energy balance model::

                >>> import climlab
                >>> model = climlab.EBM()

                >>> # initialize CO2 variable with value 280 ppm
                >>> model.add_diagnostic('CO2',280.)

                >>> # access variable directly or through diagnostic dictionary
                >>> model.CO2
                280
                >>> model.diagnostics.keys()
                ['ASR', 'CO2', 'net_radiation', 'icelat', 'OLR', 'albedo']

        """
        self._diag_vars.append(name)
        self.__setattr__(name, value)

    def add_input(self, name, value=None):
        '''Create a new input variable called ``name`` for this process
        and initialize it with the given ``value``.

        Quantity is accessible in two ways:

            * as a process attribute, i.e. ``proc.name``
            * as a member of the diagnostics dictionary,
              i.e. ``proc.diagnostics['name']``

        Use attribute method to set values, e.g.
        ```proc.name = value ```

        :param str name:        name of diagnostic quantity to be initialized
        :param array value:     initial value for quantity [default: None]
        '''
        self._input_vars.append(name)
        self.__setattr__(name, value)

    def declare_input(self, inputlist):
        '''Add the variable names in ``inputlist`` to the list of necessary inputs.'''
        for name in inputlist:
            self._input_vars.append(name)

    def remove_diagnostic(self, name):
        """	Removes a diagnostic from the ``process.diagnostic`` dictionary
        and also delete the associated process attribute.

        :param str name:    name of diagnostic quantity to be removed

        :Example:

            Remove diagnostic variable 'icelat' from energy balance model::

                >>> import climlab
                >>> model = climlab.EBM()

                >>> # display all diagnostic variables
                >>> model.diagnostics.keys()
                ['ASR', 'OLR', 'net_radiation', 'albedo', 'icelat']

                >>> model.remove_diagnostic('icelat')
                >>> model.diagnostics.keys()
                ['ASR', 'OLR', 'net_radiation', 'albedo']

                >>> # Watch out for subprocesses that may still want
                >>>  # to access the diagnostic 'icelat' variable !!!

        """
        #_ = self.diagnostics.pop(name)
        #delattr(type(self), name)
        try:
            delattr(self, name)
            self._diag_vars.remove(name)
        except:
            print 'No diagnostic named {} was found.'.format(name)

    @property
    def diagnostics(self):
        """dictionary with all diagnostic variables

        :getter:    Returns the content of ``self._diag_vars``.
        :type:      dict

        """
        return { key:value for key, value in self.__dict__.items()
                if key in self._diag_vars }
    @property
    def input(self):
        """dictionary with all input variables

        That can be boundary conditions and other gridded quantities
        independent of the `process`

        :getter:    Returns the content of ``self._input_vars``.
        :type:      dict

        """
        return { key:value for key, value in self.__dict__.items()
                 if key in self._input_vars }

    # Some handy shortcuts... only really make sense when there is only
    # a single axis of that type in the process.
    @property
    def lat(self):
        """Property of latitudinal points of the process.

        :getter:    Returns the points of axis ``'lat'`` if availible in the
                    process's domains.
        :type:      array
        :raises: :exc:`ValueError`
                    if no ``'lat'`` axis can be found.

        """
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislat = dom.axes['lat'].points
                except:
                    pass
            return thislat
        except:
            raise ValueError('Can\'t resolve a lat axis.')
    @property
    def lat_bounds(self):
        """Property of latitudinal bounds of the process.

        :getter:    Returns the bounds of axis ``'lat'`` if availible in the
                    process's domains.
        :type:      array
        :raises: :exc:`ValueError`
                    if no ``'lat'`` axis can be found.

        """
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislat = dom.axes['lat'].bounds
                except:
                    pass
            return thislat
        except:
            raise ValueError('Can\'t resolve a lat axis.')
    @property
    def lon(self):
        """Property of longitudinal points of the process.

        :getter:    Returns the points of axis ``'lon'`` if availible in the
                    process's domains.
        :type:      array
        :raises: :exc:`ValueError`
                    if no ``'lon'`` axis can be found.

        """
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislon = dom.axes['lon'].points
                except:
                    pass
            return thislon
        except:
            raise ValueError('Can\'t resolve a lon axis.')
    @property
    def lon_bounds(self):
        """Property of longitudinal bounds of the process.

        :getter:    Returns the bounds of axis ``'lon'`` if availible in the
                    process's domains.
        :type:      array
        :raises: :exc:`ValueError`
                    if no ``'lon'`` axis can be found.

        """
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislon = dom.axes['lon'].bounds
                except:
                    pass
            return thislon
        except:
            raise ValueError('Can\'t resolve a lon axis.')
    @property
    def lev(self):
        """Property of altitudinal points of the process.

        :getter:    Returns the points of axis ``'lev'`` if availible in the
                    process's domains.
        :type:      array
        :raises: :exc:`ValueError`
                    if no ``'lev'`` axis can be found.

        """
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislev = dom.axes['lev'].points
                except:
                    pass
            return thislev
        except:
            raise ValueError('Can\'t resolve a lev axis.')
    @property
    def lev_bounds(self):
        """Property of altitudinal bounds of the process.

        :getter:    Returns the bounds of axis ``'lev'`` if availible in the
                    process's domains.
        :type:      array
        :raises: :exc:`ValueError`
                    if no ``'lev'`` axis can be found.

        """
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislev = dom.axes['lev'].bounds
                except:
                    pass
            return thislev
        except:
            raise ValueError('Can\'t resolve a lev axis.')
    @property
    def depth(self):
        """Property of depth points of the process.

        :getter:    Returns the points of axis ``'depth'`` if availible in the
                    process's domains.
        :type:      array
        :raises: :exc:`ValueError`
                    if no ``'depth'`` axis can be found.

        """
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thisdepth = dom.axes['depth'].points
                except:
                    pass
            return thisdepth
        except:
            raise ValueError('Can\'t resolve a depth axis.')
    @property
    def depth_bounds(self):
        """Property of depth bounds of the process.

        :getter:    Returns the bounds of axis ``'depth'`` if availible in the
                    process's domains.
        :type:      array
        :raises: :exc:`ValueError`
                    if no ``'depth'`` axis can be found.

        """
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thisdepth = dom.axes['depth'].bounds
                except:
                    pass
            return thisdepth
        except:
            raise ValueError('Can\'t resolve a depth axis.')


def process_like(proc):
    """Copys the given process.

    The creation date is updated.

    :param proc:    process
    :type proc:     :class:`~climlab.process.process.Process`
    :return:        new process identical to the given process
    :rtype:         :class:`~climlab.process.process.Process`

    :Example:

        ::

            >>> import climlab
            >>> from climlab.process.process import process_like

            >>> model = climlab.EBM()
            >>> model.subprocess.keys()
            ['diffusion', 'LW', 'albedo', 'insolation']

            >>> albedo = model.subprocess['albedo']
            >>> albedo_copy = process_like(albedo)

            >>> albedo.creation_date
            'Thu, 24 Mar 2016 01:32:25 +0000'

            >>> albedo_copy.creation_date
            'Thu, 24 Mar 2016 01:33:29 +0000'

    """
    newproc = copy.deepcopy(proc)
    newproc.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                          time.localtime())
    return newproc


def get_axes(process_or_domain):
    """Returns a dictionary of all Axis in a domain or dictionary of domains.

    :param process_or_domain:   a process or a domain object
    :type process_or_domain:    :class:`~climlab.process.process.Process` or
                                :class:`~climlab.domain.domain._Domain`
    :raises: :exc:              `TypeError` if input is not or not having a domain
    :returns:                   dictionary of input's Axis
    :rtype:                     dict

    :Example:

        ::

            >>> import climlab
            >>> from climlab.process.process import get_axes

            >>> model = climlab.EBM()

            >>> get_axes(model)
            {'lat': <climlab.domain.axis.Axis object at 0x7ff13b9dd2d0>,
             'depth': <climlab.domain.axis.Axis object at 0x7ff13b9dd310>}

    """
    if isinstance(process_or_domain, Process):
        dom = process_or_domain.domains
    else:
        dom = process_or_domain
    if isinstance(dom, _Domain):
        return dom.axes
    elif isinstance(dom, dict):
        axes = {}
        for thisdom in dom.values():
            assert isinstance(thisdom, _Domain)
            axes.update(thisdom.axes)
        return axes
    else:
        raise TypeError('dom must be a domain or dictionary of domains.')
