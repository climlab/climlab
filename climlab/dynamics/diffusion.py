r"""General solver of the 1D diffusion equation:

.. math::

    \frac{\partial}{\partial t} \Psi(x,t) &= -\frac{1}{w(x)} \frac{\partial}{\partial x} \left[ w(x) ~ F(x,t) \right] \\
    F &= -K ~ \frac{\partial \Psi}{\partial x}

for a state variable :math:`\Psi(x,t)` and arbitrary diffusivity :math:`K(x,t)`
in units of :math:`x^2 ~ t^{-1}`.

:math:`w(x)` is an optional weighting function
for the divergence operator on curvilinear grids.

The diffusivity :math:`K` can be a single scalar,
or optionally a vector *specified at grid cell boundaries*
(so its length must be exactly 1 greater than the length of :math:`x`).

:math:`K` can be modified by the user at any time
(e.g., after each timestep, if it depends on other state variables).

A fully implicit timestep is used for computational efficiency. Thus the computed
tendency :math:`\frac{\partial \Psi}{\partial t}` will depend on the timestep.

In addition to the tendency over the implicit timestep,
the solver also calculates two diagnostics from the updated state:

- ``diffusive_flux`` given by :math:`F(x)` in units of :math:`[\Psi]~[x]`/s
- ``diffusive_flux_convergence`` given by the right hand side of the first equation above, in units of :math:`[\Psi]`/s

This base class can be used without modification for diffusion in
Cartesian coordinates (:math:`w=1`). Non-uniformly spaced grids are supported.

The state variable :math:`\Psi` may be multi-dimensional, but the diffusion
will operate along a single dimension only.

Other classes implement the weighting for spherical geometry.
"""
from __future__ import division
import numpy as np
from climlab.process.implicit import ImplicitProcess
from climlab.process.process import get_axes
from . import adv_diff_numerics


class Diffusion(ImplicitProcess):
    """A parent class for one dimensional implicit diffusion modules.

    **Initialization parameters** \n

    :param float K:                 the diffusivity parameter in units of
                                    :math:`\\frac{[\\textrm{length}]^2}{\\textrm{time}}`
                                    where length is the unit of the spatial axis
                                    on which the diffusion is occuring.
    :param str diffusion_axis:      dictionary key for axis on which the
                                    diffusion is occuring in process's domain
                                    axes dictionary
    :param bool use_banded_solver:  input flag, whether to use
                                    :py:func:`scipy.linalg.solve_banded`
                                    instead of :py:func:`numpy.linalg.solve`
                                    [default: False]

    .. note::

        The banded solver :py:func:`scipy.linalg.solve_banded` is faster than
        :py:func:`numpy.linalg.solve` but only works for one dimensional diffusion.

    **Object attributes** \n

    Additional to the parent class
    :class:`~climlab.process.implicit.ImplicitProcess`
    following object attributes are generated or modified during initialization:

    :ivar dict param:               parameter dictionary is extended by
                                    diffusivity parameter K (unit:
                                    :math:`\\frac{[\\textrm{length}]^2}{\\textrm{time}}`)
    :ivar bool use_banded_solver:   input flag specifying numerical solving
                                    method (given during initialization)
    :ivar str diffusion_axis:       dictionary key for axis where diffusion
                                    is occuring:
                                    specified during initialization
                                    or output of method
                                    :func:`_guess_diffusion_axis`
    :ivar array _K_dimensionless:    diffusion parameter K multiplied by the
                                    timestep and divided by mean of diffusion
                                    axis delta in the power of two. Array has
                                    the size of diffusion axis bounds.
                                    :math:`K_{\\textrm{dimensionless}}[i]= K \\frac{\\Delta t}{ \\left(\\overline{\\Delta \\textrm{bounds}} \\right)^2}`
    :ivar array _diffTriDiag:        tridiagonal diffusion matrix made by
                                    :func:`_make_diffusion_matrix()` with input
                                    ``self._K_dimensionless``


    :Example:

        Here is an example showing implementation of a vertical diffusion.
        It shows that a subprocess can work on just a subset of the parent process
        state variables.

            .. plot:: code_input_manual/example_diffusion.py
               :include-source:

    """
    def __init__(self,
                 K=None,
                 diffusion_axis=None,
                 use_banded_solver=False,
                 **kwargs):
        super(Diffusion, self).__init__(**kwargs)
        self.use_banded_solver = use_banded_solver
        #self.use_banded_solver = False
        if diffusion_axis is None:
            self.diffusion_axis = _guess_diffusion_axis(self)
        else:
            self.diffusion_axis = diffusion_axis
        for dom in list(self.domains.values()):
            points = dom.axes[self.diffusion_axis].points
            bounds = dom.axes[self.diffusion_axis].bounds
        self.diffusion_axis_index = dom.axis_index[self.diffusion_axis]
        #  Cell bounds and centers in length units for diffusion operator
        #  Ensure they have shame dimensions as state var
        for varname, value in self.state.items():
            arr = np.moveaxis(0.*value, self.diffusion_axis_index, -1)
            J = arr.shape[-1]
            sizeJ = tuple([n for n in arr.shape[:-1]] + [J])
            sizeJplus1 = tuple([n for n in arr.shape[:-1]] + [J+1])
            arr[...,:] = points
            self._Xcenter = arr
            self._Xbounds = np.zeros(sizeJplus1)
            self._Xbounds[...,:] = bounds
        self._weight_bounds = np.ones_like(self._Xbounds)  # weights for curvilinear grids
        self._weight_center = np.ones_like(self._Xcenter)
        self.K = K  # Diffusivity in units of [length]**2 / time
        self.add_diagnostic('diffusive_flux',
            np.moveaxis(0.*self.K*self._weight_bounds,-1,self.diffusion_axis_index))
        for varname, value in self.state.items():
            self.add_diagnostic('diffusive_flux_convergence',
                np.moveaxis(0.*self._weight_center,-1,self.diffusion_axis_index))

    @property
    def K(self):
        return self._K
    @K.setter  # currently this assumes that Kvalue is scalar or has the right dimensions...
    def K(self, Kvalue):
        self._K = Kvalue
        Karray = np.ones_like(self._Xbounds) * Kvalue
        self._diffTriDiag = adv_diff_numerics.advdiff_tridiag(X=self._Xcenter,
            Xb=self._Xbounds, K=Karray, U=0.*Karray, W=self._weight_center, Wb=self._weight_bounds,
            use_banded_solver=self.use_banded_solver)

    def _implicit_solver(self):
        """Inverts and solves the matrix problem for diffusion matrix
        and temperature T.

        The method is called by the
        :func:`~climlab.process.implicit.ImplicitProcess._compute()` function
        of the :class:`~climlab.process.implicit.ImplicitProcess` class and
        solves the matrix problem

        .. math::

            A \\cdot T_{\\textrm{new}} = T_{\\textrm{old}}

        for diffusion matrix A and corresponding temperatures.
        :math:`T_{\\textrm{old}}` is in this case the current state variable
        which already has been adjusted by the explicit processes.
        :math:`T_{\\textrm{new}}` is the new state of the variable. To
        derive the temperature tendency of the diffusion process the adjustment
        has to be calculated and muliplied with the timestep which is done by
        the :func:`~climlab.process.implicit.ImplicitProcess._compute()`
        function of the :class:`~climlab.process.implicit.ImplicitProcess`
        class.

        This method calculates the matrix inversion for every state variable
        and calling either :func:`solve_implicit_banded()` or
        :py:func:`numpy.linalg.solve()` dependent on the flag
        ``self.use_banded_solver``.

        :ivar dict state:               method uses current state variables
                                        but does not modify them
        :ivar bool use_banded_solver:   input flag whether to use
                                        :func:`_solve_implicit_banded()` or
                                        :py:func:`numpy.linalg.solve()` to do
                                        the matrix inversion
        :ivar array _diffTriDiag:        the diffusion matrix which is given
                                        with the current state variable to
                                        the method solving the matrix problem

        """
        newstate = {}
        for varname, value in self.state.items():
            field = np.moveaxis(value, self.diffusion_axis_index,-1)
            source = 0.*field
            result = adv_diff_numerics.implicit_step_forward(field,
                        self._diffTriDiag, source, self.timestep,
                        use_banded_solver=self.use_banded_solver)
            newstate[varname] = np.moveaxis(result,-1,self.diffusion_axis_index)
        return newstate

    def _update_diagnostics(self, newstate):
        Karray = self._Xbounds * self.K
        for varname, value in newstate.items():
            field = np.moveaxis(value, self.diffusion_axis_index,-1)
            #field = value.squeeze()
            flux = adv_diff_numerics.diffusive_flux(self._Xcenter,
                                        self._Xbounds, Karray, field)
            self.diffusive_flux[:] = np.moveaxis(flux,-1,self.diffusion_axis_index)
            source = 0.*field
            convergence = adv_diff_numerics.compute_tendency(field,
                self._diffTriDiag, source, use_banded_solver=self.use_banded_solver)
            self.diffusive_flux_convergence[:] = np.moveaxis(convergence,-1,self.diffusion_axis_index)


def _guess_diffusion_axis(process_or_domain):
    """Scans given process, domain or dictionary of domains for a diffusion axis
    and returns appropriate name.

    In case only one axis with length > 1 in the process or set of domains
    exists, the name of that axis is returned. Otherwise an error is raised.

    :param process_or_domain:   input from where diffusion axis should be guessed
    :type process_or_domain:    :class:`~climlab.process.process.Process`,
                                :class:`~climlab.domain.domain._Domain` or
                                :py:class:`dict` of domains
    :raises: :exc:`ValueError` if more than one diffusion axis is possible.
    :returns:                   name of the diffusion axis
    :rtype:                     str

    """
    axes = get_axes(process_or_domain)
    diff_ax = {}
    for axname, ax in axes.items():
        if ax.num_points > 1:
            diff_ax.update({axname: ax})
    if len(list(diff_ax.keys())) == 1:
        return list(diff_ax.keys())[0]
    else:
        raise ValueError('More than one possible diffusion axis.')
