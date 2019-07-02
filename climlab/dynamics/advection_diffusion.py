r"""CLIMLAB Process objects for advection-diffusion processes of the form

.. math::

    \frac{\partial}{\partial t} \psi(x,t) &= -\frac{1}{w(x)} \frac{\partial}{\partial x} \left[ w(x) ~ \mathcal{F}(x,t) \right] \\
    \mathcal{F} &= U(x) \psi(x) -K(x) ~ \frac{\partial \psi}{\partial x} + F(x)

for a state variable :math:`\psi(x,t)`, diffusivity :math:`K(x)`
in units of :math:`x^2 ~ t^{-1}`, advecting velocity :math:`U(x)`
in units of :math:`x ~ t^{-1}`, and a prescribed flux F(x)
(including boundary conditions) in units of :math:`\psi ~ x ~ t^{-1}`.

The prescribed flux :math:`F(x)` defaults to zero everywhere. The user can
implement a non-zero boundary flux condition by passing a non-zero array
``prescribed_flux`` as input.

:math:`w(x)` is an optional weighting function
for the divergence operator on curvilinear grids.

The diffusivity :math:`K` and velocity :math:`U` can be scalars,
or optionally vectors *specified at grid cell boundaries*
(so their lengths must be exactly 1 greater than the length of :math:`x`).

:math:`K` and :math:`U` can be modified by the user at any time
(e.g., after each timestep, if they depend on other state variables).

A fully implicit timestep is used for computational efficiency. Thus the computed
tendency :math:`\frac{\partial \psi}{\partial t}` will depend on the timestep.

In addition to the tendency over the implicit timestep,
the solver also calculates several diagnostics from the updated state:

- ``diffusive_flux`` given by :math:`-K(x) ~ \frac{\partial \psi}{\partial x}` in units of :math:`[\psi]~[x]`/s
- ``advective_flux`` given by :math:`U(x) \psi(x)` (same units)
- ``total_flux``, the sum of advective, diffusive and prescribed fluxes
- ``flux_convergence`` given by the right hand side of the first equation above, in units of :math:`[\psi]`/s

This base class can be used without modification for diffusion in
Cartesian coordinates (:math:`w=1`). Non-uniformly spaced grids are supported.

The state variable :math:`\psi` may be multi-dimensional, but the diffusion
will operate along a single dimension only.

Other classes implement the weighting for spherical geometry.
"""
from __future__ import division
import numpy as np
from climlab.process.implicit import ImplicitProcess
from climlab.process.process import get_axes
from . import adv_diff_numerics


class AdvectionDiffusion(ImplicitProcess):
    """A parent class for one dimensional implicit advection-diffusion modules.

    **Initialization parameters** \n

    :param float K:                 the diffusivity parameter in units of
                                    :math:`\\frac{[\\textrm{length}]^2}{\\textrm{time}}`
                                    where length is the unit of the spatial axis
                                    on which the diffusion is occuring.
    :param float U:                 Advection velocity in units of
                                    :math:`\\frac{[\\textrm{length}]}{\\textrm{time}}`
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
    :ivar array _advdiffTriDiag:        tridiagonal diffusion matrix made by
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
                 K=0.,
                 U=0.,
                 diffusion_axis=None,
                 use_banded_solver=False,
                 prescribed_flux=0.,
                 **kwargs):
        super(AdvectionDiffusion, self).__init__(**kwargs)
        self.use_banded_solver = use_banded_solver
        if diffusion_axis is None:   # diffusion axis is also advection axis!
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
        self.prescribed_flux = prescribed_flux  # flux including boundary conditions
        self.K = K  # Diffusivity in units of [length]**2 / [time]
        self.U = U  # Advecting velocity in units of [length] / [time]
        self.add_diagnostic('diffusive_flux',
            np.moveaxis(0.*self.K*self._weight_bounds,-1,self.diffusion_axis_index))
        self.add_diagnostic('advective_flux', 0.*self.diffusive_flux)
        self.add_diagnostic('total_flux', 0.*self.diffusive_flux)
        for varname, value in self.state.items():
            self.add_diagnostic('flux_convergence',
                np.moveaxis(0.*self._weight_center,-1,self.diffusion_axis_index))

    @property
    def K(self):
        return self._K
    @K.setter  # currently this assumes that Kvalue is scalar or has the right dimensions...
    def K(self, Kvalue):
        self._K = Kvalue
        self._compute_advdiff_matrix()

    @property
    def U(self):
        return self._U
    @U.setter
    def U(self, Uvalue):
        self._U = Uvalue
        self._compute_advdiff_matrix()

    @property
    def prescribed_flux(self):
        return self._prescribed_flux
    @prescribed_flux.setter
    def prescribed_flux(self, fluxvalue):
        self._prescribed_flux = fluxvalue
        for varname, value in self.state.items():
            field = np.moveaxis(value, self.diffusion_axis_index,-1)
        fluxarray = np.ones_like(self._Xbounds) * self._prescribed_flux
        self._source = adv_diff_numerics.compute_source(X=self._Xcenter,
            Xb=self._Xbounds, prescribed_flux=fluxarray,
            prescribed_source=0.*field,
            W=self._weight_center, Wb=self._weight_bounds)

    def _compute_advdiff_matrix(self):
        Karray = np.ones_like(self._Xbounds) * self.K
        try:
            Uarray = np.ones_like(self._Xbounds) * self.U
        except Exception:
            Uarray = 0.*Karray
        self._advdiffTriDiag = adv_diff_numerics.advdiff_tridiag(X=self._Xcenter,
            Xb=self._Xbounds, K=Karray, U=Uarray, W=self._weight_center, Wb=self._weight_bounds,
            use_banded_solver=self.use_banded_solver)

    def _implicit_solver(self):
        newstate = {}
        for varname, value in self.state.items():
            field = np.moveaxis(value, self.diffusion_axis_index,-1)
            result = adv_diff_numerics.implicit_step_forward(field,
                        self._advdiffTriDiag, self._source, self.timestep,
                        use_banded_solver=self.use_banded_solver)
            newstate[varname] = np.moveaxis(result,-1,self.diffusion_axis_index)
        return newstate

    def _update_diagnostics(self, newstate):
        Karray = np.ones_like(self._Xbounds) * self.K
        Uarray = np.ones_like(self._Xbounds) * self.U
        for varname, value in newstate.items():
            field = np.moveaxis(value, self.diffusion_axis_index,-1)
            diff_flux = adv_diff_numerics.diffusive_flux(self._Xcenter,
                                        self._Xbounds, Karray, field)
            adv_flux = adv_diff_numerics.advective_flux(self._Xcenter,
                                        self._Xbounds, Uarray, field)
            self.diffusive_flux[:] = np.moveaxis(diff_flux,-1,self.diffusion_axis_index)
            self.advective_flux[:] = np.moveaxis(adv_flux,-1,self.diffusion_axis_index)
            source = 0.*field
            convergence = adv_diff_numerics.compute_tendency(field,
                self._advdiffTriDiag, source, use_banded_solver=self.use_banded_solver)
            self.flux_convergence[:] = np.moveaxis(convergence,-1,self.diffusion_axis_index)


class Diffusion(AdvectionDiffusion):
    '''1D diffusion only, with advection set to zero.

    Otherwise identical to the parent class AdvectionDiffusion.
    '''
    def __init__(self,
                 K=None,
                 diffusion_axis=None,
                 use_banded_solver=False,
                 **kwargs):
        super(Diffusion, self).__init__(K=K, U=0.,
            diffusion_axis=diffusion_axis,
            use_banded_solver=use_banded_solver, **kwargs)


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
