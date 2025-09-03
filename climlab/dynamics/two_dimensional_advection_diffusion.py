r"""CLIMLAB Process objects for two-dimensional advection-diffusion processes of the form

ACTUALLY THESE NOTES ARE WRONG FOR NOW

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
"""

import numpy as np
from climlab import constants as const
from climlab.process import TimeDependentProcess


class TwoDimensionalAdvectionDiffusion(TimeDependentProcess):
    '''A solver for a 2D advection-diffusion equation of the form... (work in progress)'''

    def __init__(self,
                 Kyy=0.,
                 Kzz=0.,
                 Kyz=0.,
                 U=0.,
                 W=0.,
                 rho=0.,

                 prescribed_flux=0.,
                 interpolation_order=2,
                 use_limiters=True,
                 **kwargs):
        super(TwoDimensionalAdvectionDiffusion, self).__init__(**kwargs)
        for dom in list(self.domains.values()):
            self._phibounds = np.deg2rad(dom.axes['lat'].bounds)
            self._latbounds = np.sin(self._phibounds) * const.a
            self._dlatbounds = np.diff(self._latbounds)
            self._levbounds = dom.axes['lev'].bounds *1e2
            self._dlevbounds = np.diff(self._levbounds)
            self._latpoints = 0.5*(self._latbounds[1:] + self._latbounds[:-1])
            self._levpoints = 0.5*(self._levbounds[1:] + self._levbounds[:-1])
        for varname, value in self.state.items():
            self._tracer = value
            self._inner_tracer = self._tracer * 0
            self._tracer_integral = np.zeros(np.array(value.shape) + np.array([1,1]))
            self._istar = self._tracer_integral * 0
            self.advective_flux_yy = self._tracer_integral[:,1:] * 0
            self.advective_flux_zz = self._tracer_integral[1:,:] * 0
            self.diffusive_flux_yy = self._tracer_integral[:,1:] * 0
            self.diffusive_flux_zz = self._tracer_integral[1:,:] * 0

        self.prescribed_flux = prescribed_flux  # flux including boundary conditions
        self.interpolation_order = interpolation_order
        self.use_limiters = use_limiters
        self.U = U  # Advecting velocity in units of [length] / [time]
        self.W = W  # Advecting velocity in units of [hpa] / [time]
        self._Ud = U * 0.0 # Kyz diffusion translated to advective velocities.
        self._Wd = W * 0.0 # Kyz diffusion translated to advective velocities.
        self._Utot = U * 0.0 # total advective velocities (U + Ud)
        self._Wtot = W * 0.0 # total advective velocities (W + Wd)
        self._dt_advdiff = self.timestep
        self.rho = rho
        self.Kyy = Kyy  # Diffusivity in units of [length]**2 / [time]
        self.Kzz = Kzz  # Diffusivity in units of [length]**2 / [time]
        self.Kyz = Kyz
        self.source_param_dict = kwargs.get('source_param_dict', {})
        self.age_of_air = kwargs.get('age_of_air', 0)
        if self.age_of_air == 1:
            self.tropopause = kwargs.get('tropopause', None) * 1e2
            if (self.tropopause is None):
                raise ValueError("Input parameter 'tropopause' must not be None for age of air calculation")
            lev_2d = np.tile(self._levpoints, (len(self._latpoints), 1))
            self._under_trop = lev_2d > self.tropopause[:,None]

        fy_source_func = kwargs.get('fy_source_func', lambda obj: 0 * obj.advective_flux_yy)
        fz_source_func = kwargs.get('fz_source_func', lambda obj: 0 * obj.advective_flux_zz)
        source_func = kwargs.get('source_func', lambda obj: 0 * obj._inner_tracer)

        self.tend_fac_fz_diff = kwargs.get('tend_fac_fz_diff', 1.0)
        self.tend_fac_fy_diff = kwargs.get('tend_fac_fy_diff', 1.0)
        self.tend_fac_fz_adv = kwargs.get('tend_fac_fz_adv', 1.0)
        self.tend_fac_fy_adv = kwargs.get('tend_fac_fy_adv', 1.0)
        self.tend_fac_fz_source_func = kwargs.get('tend_fac_fz_source_func', 1.0)
        self.tend_fac_fy_source_func = kwargs.get('tend_fac_fy_source_func', 1.0)
        self.tend_fac_source_func = kwargs.get('tend_fac_source_func', 1.0)
        
        # Run once to validate
        output_y = fy_source_func(self)
        assert isinstance(output_y, np.ndarray), "fy_source_func must return a numpy array."
        assert np.all(output_y.shape == self.advective_flux_yy.shape), f"fy_source_func expected output {self.advective_flux_yy.shape}, got {output_y.shape}."
        output_z = fz_source_func(self)
        assert isinstance(output_z, np.ndarray), "fz_source_func must return a numpy array."
        assert np.all(output_z.shape == self.advective_flux_zz.shape), f"fz_source_func expected output {self.advective_flux_zz.shape}, got {output_z.shape}."
        output = source_func(self)
        assert isinstance(output, np.ndarray), "source_func must return a numpy array."
        assert np.all(output.shape == self._inner_tracer.shape), f"source_func expected output {self._inner_tracer.shape}), got {output.shape}."
        # If valid, store function
        self._internal_fy_source_func = lambda: fy_source_func(self)
        self._internal_fz_source_func = lambda: fz_source_func(self)
        self._internal_source_func = lambda: source_func(self)

    @property
    def Kyy(self):
        return self._Kyy
    @Kyy.setter  # currently this assumes that Kvalue is scalar or has the right dimensions...
    def Kyy(self, Kvalue):
        self._Kyy = Kvalue * (np.cos(self._phibounds[:, None]))**2.0

    @property
    def Kzz(self):
        return self._Kzz
    @Kzz.setter  # currently this assumes that Kvalue is scalar or has the right dimensions...
    def Kzz(self, Kvalue):
        arr = self._tracer
        J = arr.shape[-1]
        sizeJplus1 = tuple([n for n in arr.shape[:-1]] + [J+1])
        self._Kzz = Kvalue * np.ones(sizeJplus1)

    @property
    def Kyz(self):
        return self._Kyz
    @Kyz.setter  # currently this assumes that Kvalue is scalar or has the right dimensions...
    def Kyz(self, Kvalue):
        # rhom0 = 0.5*(self.rho[:, 1:] + self.rho[:, :-1])
        # rhom0 = np.append(np.append(self.rho[:, 0:1], rhom0, axis=1), self.rho[:, -1:], axis=1)
        # rhom = 0.5*(rhom0[1:, :] + rhom0[:-1, :])
        # rhom = np.append(np.append(rhom0[0:1, :], rhom, axis=0), rhom0[-1:, :], axis=0)
        self._Kyz = Kvalue * (np.cos(self._phibounds[:, None])) 

    @property
    def U(self):
        return self._U
    @U.setter
    def U(self, Uvalue):
        self._U = Uvalue * np.cos(self._phibounds[:, None])  # not clear yet how to handle this... WIP

    @property
    def W(self):
        return self._W
    @W.setter
    def W(self, Wvalue):
        arr = self._tracer
        J = arr.shape[-1]
        sizeJplus1 = tuple([n for n in arr.shape[:-1]] + [J+1])
        self._W = Wvalue * np.ones(sizeJplus1)

    def _compute(self):
        
        self._inner_tracer = self._tracer * 1e0

        self._advdiff_timestep()

        dtracer = self._inner_tracer - self._tracer

        tendencies = {}
        for name, value in self.state.items():
            tendencies[name] = dtracer / self.timestep
        return tendencies

    def _advdiff_timestep(self):
        self._boundary_conditions_tracer()
        self._mixed_diffusion_to_advection() # in principle this should be inside the small timestep loop, and then we should re-compute dt also inside that loop, but that is problematic because we start out with a given timestep...
        self._set_advdiff_dt()
        for j in range(int(self.timestep/self._dt_advdiff)):
            for i in range(2):
                self._calc_integral(i)
                self._interpolate_integral(i)
                self._compute_fluxes(i)
                self._update_field(i)
            self._compute_k_fluxes()
            self._update_field_k()

    def _calc_integral(self, i):
        if i == 0:
            self._tracer_integral[1:,0:-1] = np.cumsum(self._inner_tracer * self._dlatbounds[:, None], axis = i)
            self._tracer_integral[0,0:-1] = 0.0
        elif i == 1:
            self._tracer_integral[0:-1, 1:] = np.cumsum(self._inner_tracer * self._dlevbounds[None, :], axis = i)
            self._tracer_integral[0:-1, 0] = 0.0
        
    def _linear_interp(self, i):
        if i == 0:
            istarleft = _general_linear_interp(self._latbounds[:, None]*np.ones(self._tracer_integral[:,:-1].shape), self._tracer_integral[:,:-1], (self._latbounds[1:, None]-np.abs(self._Utot[1:,:])*self._dt_advdiff), i)
            istarleft = np.concatenate((istarleft[0:1,:]*0, istarleft), axis = i)
            istarright = _general_linear_interp(self._latbounds[:, None]*np.ones(self._tracer_integral[:,:-1].shape), self._tracer_integral[:,:-1], (self._latbounds[0:-1, None]+np.abs(self._Utot[0:-1,:])*self._dt_advdiff), i)
            istarright = np.concatenate((istarright, self._tracer_integral[-1:,:-1]), axis = i)
            self._istar[:,:-1] = np.where(self.U>0, istarleft, istarright)
        if i == 1:
            istarleft = _general_linear_interp(self._levbounds[None, :]*np.ones(self._tracer_integral[:-1,:].shape), self._tracer_integral[:-1,:], (self._levbounds[None, 1:]-np.abs(self._Wtot[:,1:])*self._dt_advdiff), i)
            istarleft = np.concatenate((istarleft[:, 0:1]*0, istarleft), axis = i)
            istarright = _general_linear_interp(self._levbounds[None, :]*np.ones(self._tracer_integral[:-1,:].shape), self._tracer_integral[:-1,:], (self._levbounds[None, 0:-1]+np.abs(self._Wtot[:, 0:-1])*self._dt_advdiff), i)
            istarright = np.concatenate((istarright, self._tracer_integral[:-1, -1:]), axis = i)
            self._istar[:-1,:] = np.where(self.W>0, istarleft, istarright)

    def _parabolic_interp(self, i):
        if i == 0:
            tracer_b = np.concatenate((np.concatenate((self._inner_tracer[0:1,:],self._inner_tracer), axis=0), self._inner_tracer[-1:,:]), axis=0)
            dy_b = np.append(np.append(self._dlatbounds[0], self._dlatbounds), self._dlatbounds[-1])
            self._istar[:,:-1] = self._tracer_integral[:, :-1] - (dy_b[1:, None] * tracer_b[:-1, :] + dy_b[:-1, None] * tracer_b[1:, :]) / (dy_b[1:, None] + dy_b[:-1, None]) * self._Utot * self._dt_advdiff + np.diff(tracer_b, axis = 0)*(self._Utot * self._dt_advdiff)**2 / (dy_b[1:, None] + dy_b[:-1, None])
        elif i == 1:
            tracer_b = np.concatenate((np.concatenate((self._inner_tracer[:,0:1],self._inner_tracer), axis=1), self._inner_tracer[:,-1:]), axis=1)
            dx_b = np.append(np.append(self._dlevbounds[0], self._dlevbounds), self._dlevbounds[-1])
            self._istar[:-1,:] = self._tracer_integral[:-1, :] - (dx_b[None, 1:] * tracer_b[:, :-1] + dx_b[None, :-1] * tracer_b[:, 1:]) / (dx_b[None, 1:] + dx_b[None, :-1]) * self._Wtot * self._dt_advdiff + np.diff(tracer_b, axis = 1)*(self._Wtot * self._dt_advdiff)**2 / (dx_b[None, 1:] + dx_b[None, :-1])
        if self.use_limiters:
            self._limiters(i)

    def _limiters(self, i):
        if i == 0:
            icenter = _general_linear_interp(self._latbounds[:, None]*np.ones(self._tracer_integral[:,:-1].shape), self._tracer_integral[:,:-1], (self._latbounds[1:, None]-np.abs(self._Utot[1:,:])*self._dt_advdiff), i)
            iright = _general_linear_interp(self._latbounds[:, None]*np.ones(self._tracer_integral[:,:-1].shape), self._tracer_integral[:,:-1], (self._latbounds[:-1, None]-np.abs(self._Utot[:-1,:])*self._dt_advdiff), i)
            ileft = _general_linear_interp(self._latbounds[:-1, None]*np.ones(self._tracer_integral[:-1,:-1].shape), self._tracer_integral[:-1,:-1], (self._latbounds[2:, None]-np.abs(self._Utot[2:,:])*self._dt_advdiff), i)
            ileft = np.append(ileft[0:1, :], ileft, axis=i)
            iright = np.append(iright[1:, :], iright[-1:, :], axis=i)
            imin = np.minimum(icenter, np.maximum(ileft, iright))
            imax = np.maximum(icenter, np.minimum(ileft, iright))
            istarleft = np.append(np.zeros(self._istar[0:1, :-1].shape), np.minimum(imax, np.maximum(self._istar[1:, :-1], imin)), axis = i)

            icenter = _general_linear_interp(self._latbounds[:, None]*np.ones(self._tracer_integral[:,:-1].shape), self._tracer_integral[:,:-1], (self._latbounds[:-1, None]+np.abs(self._Utot[:-1,:])*self._dt_advdiff), i)
            iright = _general_linear_interp(self._latbounds[1:, None]*np.ones(self._tracer_integral[1:,:-1].shape), self._tracer_integral[1:,:-1], (self._latbounds[:-2, None]+np.abs(self._Utot[:-2,:])*self._dt_advdiff), i)
            ileft = _general_linear_interp(self._latbounds[:, None]*np.ones(self._tracer_integral[:,:-1].shape), self._tracer_integral[:,:-1], (self._latbounds[1:, None]+np.abs(self._Utot[1:,:])*self._dt_advdiff), i)
            ileft = np.append(ileft[0:1, :], ileft[:-1, :], axis=i)
            iright = np.append(iright, iright[-1:, :], axis=i)
            imin = np.minimum(icenter, np.maximum(ileft, iright))
            imax = np.maximum(icenter, np.minimum(ileft, iright))
            istarright = np.minimum(imax, np.maximum(self._istar[0:-1, :-1], imin))
            istarright = np.append(istarright, istarright[-1:, :], axis=i)        
            self._istar[:,:-1] = np.where(self._Utot>0, istarleft, istarright)
        elif i == 1:
            icenter = _general_linear_interp(self._levbounds[None, :]*np.ones(self._tracer_integral[:-1,:].shape), self._tracer_integral[:-1,:], (self._levbounds[None, 1:]-np.abs(self._Wtot[:,1:])*self._dt_advdiff), i)
            iright = _general_linear_interp(self._levbounds[None, :]*np.ones(self._tracer_integral[:-1,:].shape), self._tracer_integral[:-1,:], (self._levbounds[None, :-1]-np.abs(self._Wtot[:,:-1])*self._dt_advdiff), i)
            ileft = _general_linear_interp(self._levbounds[None, :-1]*np.ones(self._tracer_integral[:-1,:-1].shape), self._tracer_integral[:-1,:-1], (self._levbounds[None, 2:]-np.abs(self._Wtot[:,2:])*self._dt_advdiff), i)
            ileft = np.append(ileft[:, 0:1], ileft, axis=i)
            iright = np.append(iright[:, 1:], iright[:, -1:], axis=i)
            imin = np.minimum(icenter, np.maximum(ileft, iright))
            imax = np.maximum(icenter, np.minimum(ileft, iright))
            istarleft = np.append(np.zeros(self._istar[:-1, 0:1].shape), np.minimum(imax, np.maximum(self._istar[:-1, 1:], imin)), axis = i)

            icenter = _general_linear_interp(self._levbounds[None, :]*np.ones(self._tracer_integral[:-1,:].shape), self._tracer_integral[:-1,:], (self._levbounds[None, :-1]+np.abs(self._Wtot[:,:-1])*self._dt_advdiff), i)
            iright = _general_linear_interp(self._levbounds[None, 1:]*np.ones(self._tracer_integral[:-1,1:].shape), self._tracer_integral[:-1,1:], (self._levbounds[None, :-2]+np.abs(self._Wtot[:,:-2])*self._dt_advdiff), i)
            ileft = _general_linear_interp(self._levbounds[None, :]*np.ones(self._tracer_integral[:-1,:].shape), self._tracer_integral[:-1,:], (self._levbounds[None, 1:]+np.abs(self._Wtot[:,1:])*self._dt_advdiff), i)
            ileft = np.append(ileft[:, 0:1], ileft[:, :-1], axis=i)
            iright = np.append(iright, iright[:, -1:], axis=i)
            imin = np.minimum(icenter, np.maximum(ileft, iright))
            imax = np.maximum(icenter, np.minimum(ileft, iright))
            istarright = np.minimum(imax, np.maximum(self._istar[:-1, 0:-1], imin))
            istarright = np.append(istarright, istarright[:, -1:], axis=i)        
            self._istar[:-1,:] = np.where(self._Wtot>0, istarleft, istarright)
        
    def _interpolate_integral(self, i):
        if self.interpolation_order == 1:
            self._linear_interp(i)
        elif self.interpolation_order == 2:
            self._parabolic_interp(i)

    def _compute_fluxes(self, i):
        if i == 0:
            # rho_up = np.where(self.U[1:-1,:]>0, self.rho[:-1,:], self.rho[1:,:])
            # rho_up = np.concatenate((np.concatenate((rho_up[0:1,:],rho_up), axis=0), rho_up[-1:,:]), axis=0)
            self.advective_flux_yy = (self._tracer_integral[:,:-1] - self._istar[:,:-1]) / self._dt_advdiff #* rho_up
        if i == 1:
            self.advective_flux_zz = -(self._tracer_integral[:-1,:] - self._istar[:-1,:]) / self._dt_advdiff
        self._boundary_conditions_flux(i)

    def _compute_k_fluxes(self):
        tracer_b = np.concatenate((np.concatenate((self._inner_tracer[0:1,:],self._inner_tracer), axis=0), self._inner_tracer[-1:,:]), axis=0)
        dy_b = np.append(np.append(self._dlatbounds[0], self._dlatbounds), self._dlatbounds[-1])
        dym = 0.5*(dy_b[1:] + dy_b[:-1])
        self.diffusive_flux_yy = -self.Kyy * np.diff(tracer_b, axis = 0)/dym[:, None] #* rhomy
        self.diffusive_flux_yy[0, :] = 0.0
        self.diffusive_flux_yy[-1, :] = 0.0
        tracer_b = np.concatenate((np.concatenate((self._inner_tracer[:,0:1],self._inner_tracer), axis=1), self._inner_tracer[:,-1:]), axis=1)
        dz_b = np.append(np.append(self._dlevbounds[0], self._dlevbounds), self._dlevbounds[-1])
        dzm = 0.5*(dz_b[1:] + dz_b[:-1])
        self.diffusive_flux_zz = -self.Kzz * np.diff(tracer_b, axis = 1)/dzm[None, :] #* rhomz
        self.diffusive_flux_zz[:,0] = 0.0
        self.diffusive_flux_zz[:,-1] = 0.0

    # Mixed diffusion (Y-Z) as implemented in MALTA scheme (Western et. al. '24, https://github.com/lukewestern/malta)
    def _mixed_diffusion_to_advection(self):
        epsilon = 1e-2
        max_tracer = np.max(self._inner_tracer)

        tracer_bz = np.concatenate((np.concatenate((self._inner_tracer[:,0:1],self._inner_tracer), axis=1), self._inner_tracer[:,-1:]), axis=1)
        tracer_by = np.concatenate((np.concatenate((self._inner_tracer[0:1,:],self._inner_tracer), axis=0), self._inner_tracer[-1:,:]), axis=0)

        if np.max(np.abs(self.Kyz)) > 0.0:
            k_y_zmid = 0.5*(self.Kyz[:,:-1] + self.Kyz[:,1:])
            tracer_edges = 0.5*(tracer_bz[:,:-1] + tracer_bz[:,1:])
            dqdz = np.diff(tracer_edges, axis=1) / self._dlevbounds[None, :]
            dqdz = np.concatenate((np.concatenate((dqdz[0:1,:],dqdz), axis=0), dqdz[-1:,:]), axis=0)
            dqdz = 0.5*(dqdz[1:,:] + dqdz[:-1,:])
            tracer_m = 0.5*(tracer_by[:-1,:] + tracer_by[1:,:])
            self._Ud = k_y_zmid * dqdz / tracer_m
            self._Ud = np.where(tracer_m < epsilon * max_tracer, 0, self._Ud)
        else:
            self._Ud = self.U * 0.0
        self._Utot = self.U + self._Ud

        if np.max(np.abs(self.Kyz)) > 0.0:
            k_ymid_z = 0.5*(self.Kyz[:-1,:] + self.Kyz[1:,:])
            tracer_edges = 0.5*(tracer_by[:-1,:] + tracer_by[1:,:])
            dqdy = np.diff(tracer_edges, axis=0) / self._dlatbounds[:, None]
            dqdy = np.concatenate((np.concatenate((dqdy[:, 0:1],dqdy), axis=1), dqdy[:,-1:]), axis=1)
            dqdy = 0.5*(dqdy[:,1:] + dqdy[:,:-1])
            tracer_m = 0.5*(tracer_bz[:,:-1] + tracer_bz[:,1:])
            self._Wd = k_ymid_z * dqdy / tracer_m
            self._Wd = np.where(tracer_m < epsilon * max_tracer, 0, self._Wd)
        else:
            self._Wd = self.W * 0.0
        self._Wtot = self.W + self._Wd
        
    def _update_field_k(self):
        self._inner_tracer += self.tend_fac_fy_diff * (self.diffusive_flux_yy[:-1,:] - self.diffusive_flux_yy[1:,:]) * self._dt_advdiff / self._dlatbounds[:, None]
        self._inner_tracer += self.tend_fac_fz_diff * (self.diffusive_flux_zz[:,:-1] - self.diffusive_flux_zz[:,1:]) * self._dt_advdiff / self._dlevbounds[None, :]
        self._inner_tracer += self.tend_fac_source_func * self._internal_source_func() * self._dt_advdiff

        # tracer += (kxx_flux[:-1, :] - kxx_flux[1:, :]) / (rho * dx)
        # tracer += (kyy_flux[:, :-1] - kyy_flux[:, 1:]) / (rho * dy)
        # tracer += (kxy_flux1[:, 1:] - kxy_flux1[:, :-1]) / (rho * dy)
        # tracer += (kxy_flux2[1:, :] - kxy_flux2[:-1, :]) / (rho * dx)
        # tracer_b[1:-1, 1:-1] = tracer
        
    def _boundary_conditions_tracer(self):
        if self.age_of_air == 1:
            self._inner_tracer[self._under_trop] = self.time['days_elapsed'] / 365.0

    def _boundary_conditions_flux(self, i):
        if i == 0:
            self.advective_flux_yy[0,:] = 0.0
            self.advective_flux_yy[-1,:] = 0.0
        elif i == 1:
            self.advective_flux_zz[:,0] = 0.0
            self.advective_flux_zz[:, -1] = 0.0

    def _boundary_conditions_kflux():
        temp = 0

    def _update_field(self, i):
        if i == 0:
            self._inner_tracer += self.tend_fac_fy_adv * (self.advective_flux_yy[:-1,:] - self.advective_flux_yy[1:,:]) / (self._dlatbounds[:, None]) * self._dt_advdiff
            fy = self._internal_fy_source_func()
            self._inner_tracer += self.tend_fac_fy_source_func * (fy[:-1,:] - fy[1:,:]) / (self._dlatbounds[:, None]) * self._dt_advdiff
        if i == 1:
            self._inner_tracer += self.tend_fac_fz_adv * (self.advective_flux_zz[:,:-1] - self.advective_flux_zz[:,1:]) / (-self._dlevbounds[None, :]) * self._dt_advdiff
            fz = self._internal_fz_source_func()
            self._inner_tracer += self.tend_fac_fz_source_func * (fz[:,:-1] - fz[:,1:]) / (-self._dlevbounds[None, :]) * self._dt_advdiff
        # global tracer, tracer_b
        # if i == 0:
        #     # tracer += (flux[:-1, :] - flux[1:, :]) / (rhop * dx)
        #     tracer += (flux[:-1, :] - flux[1:, :]) / (-dx/g)
        # elif i == 1:
        #     tracer += (flux[:, :-1] - flux[:, 1:]) / (rho * dy)
        # tracer_b[1:-1, 1:-1] = tracer

    def _set_advdiff_dt(self):
        dtlat = dtlev = dtkz = dtky = dtkxy = 1e20
        courant = 0.5
        dlatm = np.append(np.append(self._dlatbounds[0],np.minimum(self._dlatbounds[:-1], self._dlatbounds[1:])),self._dlatbounds[-1])
        dtlat = (np.abs(dlatm[:, None] / (self._Utot+1e-20))).min()*courant
        dlevm = np.append(np.append(self._dlevbounds[0],np.minimum(self._dlevbounds[:-1], self._dlevbounds[1:])),self._dlevbounds[-1])
        dtlev = (np.abs(dlevm[None, :] / (self._Wtot+1e-20))).min()*courant
        if self.Kyy.max()>0:
            dy_b = np.append(np.append(self._dlatbounds[0], self._dlatbounds), self._dlatbounds[-1])
            dym = 0.5*(dy_b[1:] + dy_b[:-1])
            dtky = (dym[:, None]**2/2/(self.Kyy+1e-9)).min()
        if self.Kzz.max()>0:
            dz_b = np.append(np.append(self._dlevbounds[0], self._dlevbounds), self._dlevbounds[-1])
            dzm = 0.5*(dz_b[1:] + dz_b[:-1])
            dtkz = (dzm[None, :]**2/2/(self.Kzz+1e-9)).min()
        fac = 0.5
        dt = min(dtlat*fac, dtlev*fac, dtkz*fac, dtky*fac, dtkxy*fac, self.timestep)
        nsteps = np.ceil(self.timestep / dt)
        self._dt_advdiff = self.timestep / nsteps


def _general_linear_interp(xvec, vec, location, i):
    # # this function receives locations vector and y vectors (size N) and a set of new locations (size N-1) and returns the linearized values 
    # # at the new locations.
    if i == 0:
        # linvec = vec[:-1, :] * (1.0 - location) + vec[1:, :] * location
        linvec = vec[:-1, :] + np.diff(vec, axis = i) * (location - xvec[:-1, :]) / np.diff(xvec, axis = i)
    if i == 1:
        linvec = vec[:, :-1] + np.diff(vec, axis = i) * (location - xvec[:, :-1]) / np.diff(xvec, axis = i)
    return linvec       


class ParticleSource(TimeDependentProcess):
    def __init__(self, source_type=0, rho=0, **kwargs):
        super(ParticleSource, self).__init__(**kwargs)
        self.source_type = source_type
        for dom in list(self.domains.values()):
            self._phibounds = np.deg2rad(dom.axes['lat'].bounds)
            self._latbounds = np.sin(self._phibounds) * const.a
            self._dlatbounds = np.diff(self._latbounds)
            self._levbounds = dom.axes['lev'].bounds *1e2
            self._dlevbounds = np.diff(self._levbounds)
            self._latpoints = 0.5*(self._latbounds[1:] + self._latbounds[:-1])
            self._levpoints = 0.5*(self._levbounds[1:] + self._levbounds[:-1])
            assert 'lat_range' in kwargs, 'lat_range must be provided when source_type=0'
            assert 'lev_range' in kwargs, 'lev_range must be provided when source_type=0'
            self.lat_range = np.sin(np.deg2rad(kwargs['lat_range'])) * const.a
            self.lev_range = kwargs['lev_range'] *1e2
            self.rho = rho
            dz = self._dlevbounds[None, :] / (const.g * self.rho)
            z = const.a + np.cumsum(dz[:,::-1], axis = 1)[:,::-1]
            z = np.concatenate((z, z[:,0:1]*0.0 + const.a), axis = 1)
            self.volume = -2*np.pi/3*(np.diff(z**3, axis = 1))*np.diff(np.sin(self._phibounds))[:, None]
            p0 = np.argmin(np.abs(self._levpoints-self.lev_range[0]))
            p1 = np.argmin(np.abs(self._levpoints-self.lev_range[-1])) + 1
            lat0 = np.argmin(np.abs(self._latpoints-self.lat_range[0]))
            lat1 = np.argmin(np.abs(self._latpoints-self.lat_range[-1])) + 1
            self._lat_range_index = np.array([lat0, lat1])
            self._lev_range_index = np.array([p0, p1])

        if source_type == 1:
            # Constant source, in kg per year
            self.rate = kwargs['rate']

    def _compute(self):

        mp = 4/3*np.pi*0.5e-6**3*2100
        mass_fac = mp / (28.96e-3/6.02e23)

        M_step = self.rate / (365 * 86400) * self.timestep
        M_air = (self.rho * self.volume)[self._lat_range_index[0]:self._lat_range_index[1],self._lev_range_index[0]:self._lev_range_index[1]]
        frac_air = M_air / sum(M_air)
        frac_step = M_step * frac_air
        x_plus = frac_step / M_air / mass_fac

        tendencies = {}
        for name, value in self.state.items():
            dtracer = value * 0
            dtracer[self._lat_range_index[0]:self._lat_range_index[1],self._lev_range_index[0]:self._lev_range_index[1]] = x_plus
            tendencies[name] = dtracer / self.timestep
        return tendencies


class ParticleSink(TimeDependentProcess):
    # copied from "ConvectiveAdjustment"
    #Hard Adjustment to a prescribed particle sink.

    def __init__(self, tropopause_p=None, **kwargs):
        super(ParticleSink, self).__init__(**kwargs)
        # tropopause definition, in hPa
        self.tropopause_p = tropopause_p
        self.time_type = 'adjustment'
        self.adjustment = {}
        for dom in list(self.domains.values()):
            self._levbounds = dom.axes['lev'].bounds *1e2
            self._dlevbounds = np.diff(self._levbounds)
            self._levpoints = 0.5*(self._levbounds[1:] + self._levbounds[:-1])
        self.under_tropopause = self._levpoints[None, :] > self.tropopause_p[:, None]
    def _compute(self):
        # print("in sink compute")
        for name, value in self.state.items():
            # print("in for loop with name"+name)
            if self.tropopause_p is None:
                self.adjustment[name] = value * 0.
                # print("tropopause_p is None")
            else:
                # print("fix being made")
                self.adjustment[name] = np.where(self.under_tropopause, -value, 0.0)

        #  return the adjustment, independent of timestep
        #  because the parent process might have set a different timestep!
        return self.adjustment
