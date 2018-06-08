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
Cartesian coordinates (:math:`w=1`) on a regularly spaced grid.

The state variable :math:`\Psi` may be multi-dimensional, but the diffusion
will operate along a single dimension only.

Other classes implement the weighting for spherical geometry.
"""
from __future__ import division
import numpy as np
from scipy.linalg import solve_banded
from climlab.process.implicit import ImplicitProcess
from climlab.process.process import get_axes


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
        if diffusion_axis is None:
            self.diffusion_axis = _guess_diffusion_axis(self)
        else:
            self.diffusion_axis = diffusion_axis
        # This currently only works with evenly spaced points
        for dom in list(self.domains.values()):
            points = dom.axes[self.diffusion_axis].points
            delta = np.mean(dom.axes[self.diffusion_axis].delta)
            bounds = dom.axes[self.diffusion_axis].bounds
        self.diffusion_axis_index = dom.axis_index[self.diffusion_axis]
        self.delta = delta  # grid interval in length units
        self._weight1 = np.ones_like(bounds)  # weights for curvilinear grids
        self._weight2 = np.ones_like(points)
        self.K = K  # Diffusivity in units of [length]**2 / time
        # These diagnostics currently only implemented for 1D state variable
        self.add_diagnostic('diffusive_flux', 0.*self._K_dimensionless*self._weight1)
        self.add_diagnostic('diffusive_flux_convergence',
                0.*np.diff(self.diffusive_flux, axis=self.diffusion_axis_index))

    @property
    def K(self):
        return self._K
    @K.setter
    def K(self, Kvalue):
        self._K = Kvalue
        # This currently only works with evenly spaced points
        for dom in list(self.domains.values()):
            bounds = dom.axes[self.diffusion_axis].bounds
        self._K_dimensionless = (Kvalue * np.ones_like(bounds) *
                                self.timestep / self.delta**2)
        self._diffTriDiag = _make_diffusion_matrix(self._K_dimensionless,
                                        self._weight1, self._weight2)

    def _implicit_solver(self):
        """Invertes and solves the matrix problem for diffusion matrix
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
        #if self.update_diffusivity:

        # Time-stepping the diffusion is just inverting this matrix problem:
        newstate = {}
        for varname, value in self.state.items():
            if self.use_banded_solver:
                newvar = _solve_implicit_banded(value, self._diffTriDiag)
            else:
                newvar = np.linalg.solve(self._diffTriDiag, value)
            newstate[varname] = newvar
        return newstate

    def _update_diagnostics(self, newstate):
        for varname, value in newstate.items():
            if np.squeeze(value).ndim == 1:
                # Diagnostic calculations of flux and convergence
                #  These assume 1D state variables...
                ax = self.state[varname].domain.axis_index[self.diffusion_axis] # axis index
                K = self._K_dimensionless * self.delta**2/self.timestep  # length**2 / time

                self.diffusive_flux[1:-1] = -K[1:-1] * np.diff(np.squeeze(value), axis=ax)/self.delta  # length / time
                self.diffusive_flux_convergence[:] = -np.diff(self.diffusive_flux*self._weight1, axis=ax)/self.delta/self._weight2 # 1/time
            else:
                pass  # To do: implement flux diagnostics for arbitrary dimension state variables


def _make_diffusion_matrix(K, weight1=None, weight2=None):
    """Builds the general diffusion matrix with dimension nxn.

    .. note::

        :math:`n`   = number of points of diffusion axis
        :math:`n+1` = number of bounts of diffusion axis


    **Function-all argument** \n

    :param array K:         dimensionless diffusivities at cell boundaries
                            *(size: 1xn+1)*
    :param array weight1:   weight_1 *(size: 1xn+1)*
    :param array weight2:   weight_2 *(size: 1xn)*
    :returns:               completely listed tridiagonal diffusion matrix *(size: nxn)*
    :rtype:                 array

    .. note::

        The elements of array K are acutally dimensionless:

        .. math::

            K[i] = K_{\\textrm{physical}}  \\frac{\\Delta t}{(\\Delta y)^2}

        where :math:`K_{\\textrm{physical}}` is in unit :math:`\\frac{\\textrm{length}^2}{\\textrm{time}}`


    The diffusion matrix is build like the following

    .. math::

        \\textrm{diffTriDiag}=
        \\left[ \\begin{array}{cccccc}
        1+\\frac{s_1 }{w_{2,0}} & -\\frac{s_1}{w_{2,0}} & 0 &  & ... & 0  \\\\
        -\\frac{s_1}{w_{2,1}} & 1+\\frac{s_1 + s_2}{w_{2,1}} & -\\frac{s_2}{w_{2,1}} & 0 & ... & 0 \\\\
        0 & -\\frac{s_2}{w_{2,2}}  & 1+\\frac{s_2 + s_3}{w_{2,2}} & -\\frac{s_3}{w_{2,2}} &... & 0  \\\\
          &  & \\ddots & \\ddots & \\ddots & \\\\
        0 & 0 & ... & -\\frac{s_{n-2}}{w_{2,n-2}}  & 1+\\frac{s_{n-2} + s_{n-1}}{w_{2,{n-2}}} & -\\frac{s_{n-1}}{w_{2,{n-2}}} \\\\
        0 & 0 & ... & 0 & -\\frac{s_{n-1}}{w_{2,n-1}}  & 1+\\frac{s_{n-1}}{w_{2,n-1}} \\\\
        \\end{array} \\right]

    where

    .. math::

           \\begin{array}{lllllll}
                K   &= [K_0,     &K_1,    &K_2,    &...,&K_{n-1},  &K_{n}] \\\\
                w_1 &= [w_{1,0}, &w_{1,1},&w_{1,2},&...,&w_{1,n-1},&w_{1,n}] \\\\
                w_2 &= [w_{2,0}, &w_{2,1},&w_{2,2},&...,&w_{2,n-1}]
           \\end{array}

    and following subsitute:

    .. math::

        s_i = w_{1,i} K_i

    """

#           \\begin{eqnarray}
#              y    & = & ax^2 + bx + c \\\\
#              f(x) & = & x^2 + 2xy + y^2
#           \\end{eqnarray}

#    .. math::
#
#        K   &= [K_0,        &K_1,        &K_2,        &...    ,    &K_{n-1},      &K_{n}] \\\\
#        w_1 &= [w_{1,0},    &w_{1,1},    &w_{1,2},    &...    ,    &w_{1,n-1}, \\ &w_{1,n}] \\\\
#        w_2 &= [w_{2,0}, \\ &w_{2,1}, \\ &w_{2,2}, \\ &... \\ , \\ &w_{2,n-1}]    &o \\\\
#
#    """
    J = K.size - 1
    if weight1 is None:
        weight1 = np.ones_like(K)
    if weight2 is None:
        weight2 = np.ones(J)
    weightedK = weight1 * K
    Ka1 = weightedK[0:J] / weight2
    Ka3 = weightedK[1:J+1] / weight2
    Ka2 = np.insert(Ka1[1:J], 0, 0) + np.append(Ka3[0:J-1], 0)
    #  Atmosphere tridiagonal matrix
    #  this code makes a 3xN matrix, suitable for use with solve_banded
    #diag = np.empty((3, J))
    #diag[0, 1:] = -Ka3[0:J-1]
    #diag[1, :] = 1 + Ka2
    #diag[2, 0:J-1] = -Ka1[1:J]
    #  Build the full banded matrix instead
    A = (np.diag(1 + Ka2, k=0) +
         np.diag(-Ka3[0:J-1], k=1) +
         np.diag(-Ka1[1:J], k=-1))
    return A


def _make_meridional_diffusion_matrix(K, lataxis):
    """Calls :func:`_make_diffusion_matrix` with appropriate weights for
    the meridional diffusion case.

    :param array K:         dimensionless diffusivities at cell boundaries
                            of diffusion axis ``lataxis``
    :param axis lataxis:    latitude axis where diffusion is occuring

    Weights are computed as the following:

    .. math::

        \\begin{array}{ll}
            w_1 &= \\cos(\\textrm{bounds}) \\\\
                &= \\left[ \\cos(b_0), \\cos(b_1), \\cos(b_2), \\ ... \\ , \\cos(b_{n-1}), \\cos(b_n) \\right] \\\\
            w_2 &= \\cos(\\textrm{points}) \\\\
                &= \\left[ \\cos(p_0), \\cos(p_1), \\cos(p_2), \\ ... \\ , \\cos(p_{n-1}) \\right]
        \\end{array}

    when bounds and points from ``lataxis`` are written as

    .. math::

        \\begin{array}{ll}
            \\textrm{bounds}   &= [b_0, b_1, b_2, \\ ... \\ , b_{n-1}, b_{n}] \\\\
            \\textrm{points}   &= [p_0, p_1, p_2, \\ ... \\ , p_{n-1}]
        \\end{array}

    Giving this input to :func:`_make_diffusion_matrix` results in a matrix like:

    .. math::

        \\textrm{diffTriDiag}=
        \\left[ \\begin{array}{cccccc}
        1+\\frac{u_1 }{\\cos(p_0)} & -\\frac{u_1}{\\cos(p_0)} & 0 &  & ... & 0  \\\\
        -\\frac{u_1}{\\cos(p_1)} & 1+\\frac{u_1 + u_2}{\\cos(p_1)} & -\\frac{u_2}{\\cos(b_1)} & 0 & ... & 0 \\\\
        0 & -\\frac{u_2}{\\cos(p_2)}  & 1+\\frac{u_2 + u_3}{\\cos(p_2)} & -\\frac{u_3}{\\cos(p_2)} &... & 0  \\\\
          &  & \\ddots & \\ddots & \\ddots & \\\\
        0 & 0 & ... & -\\frac{u_{n-2}}{\\cos(p_{n-2})}  & 1+\\frac{u_{n-2} + u_{n-1}}{\\cos(p_{n-2})} & -\\frac{u_{n-1}}{\\cos(p_{n-2})} \\\\
        0 & 0 & ... & 0 & -\\frac{u_{n-1}}{\\cos(p_{n-1})}  & 1+\\frac{u_{n-1}}{\\cos(p_{n-1})} \\\\
        \\end{array} \\right]

    with the substitue of:

    .. math::

        u_i = \\cos(b_i) K_i

    """
    phi_stag = np.deg2rad(lataxis.bounds)
    phi = np.deg2rad(lataxis.points)
    weight1 = np.cos(phi_stag)
    weight2 = np.cos(phi)
    diag = _make_diffusion_matrix(K, weight1, weight2)
    return diag, weight1, weight2


def _solve_implicit_banded(current, banded_matrix):
    """Uses a banded solver for matrix inversion of a tridiagonal matrix.

    Converts the complete listed tridiagonal matrix *(nxn)* into a three row
    matrix *(3xn)* and calls :py:func:`scipy.linalg.solve_banded()`.

    :param array current:           the current state of the variable for which
                                    matrix inversion should be computed
    :param array banded_matrix:     complete diffusion matrix (*dimension: nxn*)
    :returns:                       output of :py:func:`scipy.linalg.solve_banded()`
    :rtype:                         array

    """
    #  can improve performance by storing the banded form once and not
    #  recalculating it...
    #  but whatever
    J = banded_matrix.shape[0]
    diag = np.zeros((3, J))
    diag[1, :] = np.diag(banded_matrix, k=0)
    diag[0, 1:] = np.diag(banded_matrix, k=1)
    diag[2, :-1] = np.diag(banded_matrix, k=-1)
    return solve_banded((1, 1), diag, current)


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
