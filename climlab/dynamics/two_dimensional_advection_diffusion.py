#  Clarify the spherical geometrical factors in the documented equation
r"""Two-dimensional advection-diffusion transport for atmospheric tracers.

This module implements transport of atmospheric tracers using a split-operator
advection-diffusion scheme in latitude (y) and pressure (p) coordinates.

The transport equation solved is (from Miller 1981, Western 2024):

.. math::

    \frac{\partial \bar{\chi}}{\partial t} + v^{adv}_y \frac{\partial \bar{\chi}}{\partial y}
    + w^{adv} \frac{\partial \bar{\chi}}{\partial p} = \bar{S}_\chi
    + \frac{\partial}{\partial y}\left(K_{yy} \frac{\partial \bar{\chi}}{\partial y}\right)
    + \frac{\partial}{\partial p}\left(K_{pp} \frac{\partial \bar{\chi}}{\partial p}\right)
    + \text{mixed derivative terms}

The advection is computed using the NIRVANA scheme (Leonard 1995, Gregory 2002),
which uses a cumulative integral formulation with parabolic interpolation
and monotonicity limiters.

References
----------
.. [1] Leonard, B.P. (1995). "NIRVANA" scheme.
.. [2] Gregory, D. (2002). Q. J. R. Meteorol. Soc.
.. [3] Miller, M. (1981). Two-dimensional transport.
.. [4] Western, L. (2024). MALTA scheme for mixed diffusion.
"""
from __future__ import division
from builtins import range
import numpy as np
from climlab import constants as const
from climlab.process.time_dependent_process import TimeDependentProcess
from climlab.domain.field import Field
from climlab.dynamics import two_d_adv_diff_numerics as numerics

# Named constants for numerical stability
MIXED_DIFFUSION_EPSILON = 1e-2  # Cutoff for Kyz effective velocity
VELOCITY_EPSILON = 1e-20  # Avoid division by zero in timestep calc
DIFFUSIVITY_EPSILON = 1e-9  # Avoid division by zero in timestep calc
CFL_ADVECTION = 0.5  # Courant number for advection
CFL_DIFFUSION = 0.5  # Safety factor for diffusion timestep
### Some of these are not being used currently.
### Also need to be propertly documented

class TwoDimensionalAdvectionDiffusion(TimeDependentProcess):
    r"""Two-dimensional advection-diffusion transport process.

    Solves the zonally-averaged tracer transport equation using split-operator
    advection (NIRVANA scheme) and explicit diffusion.

    Parameters
    ----------
    Kyy : float or ndarray
        Meridional diffusivity in m²/s. Shape (nlat+1, nlev) at lat boundaries.
    Kzz : float or ndarray
        Vertical diffusivity in m²/s. Shape (nlat, nlev+1) at lev boundaries.
    Kyz : float or ndarray
        Mixed (off-diagonal) diffusivity in m²/s. Shape (nlat+1, nlev+1).
    U : float or ndarray
        Meridional advecting velocity in m/s. Shape (nlat+1, nlev).
    W : float or ndarray
        Vertical advecting velocity in Pa/s. Shape (nlat, nlev+1).
    W_sedimentation : float or ndarray
        Additional vertical sedimentation velocity in Pa/s.
    interpolation_order : int
        Order of interpolation: 1 for linear, 2 for parabolic (default: 2).
    use_limiters : bool
        Whether to apply monotonicity limiters (default: True).
    diagnostic_name_suffix : str
        Suffix for diagnostic variable names.
    """

## Docs must clarify that the independent coordinate in the latitude dimension is
## y = a sin(phi)
##  so the differential is dy = a cos(phi) dphi 
## with phi the latitude in radians
##   Miller (1981)
##  "equal increments in y correspond to equal increments in global surface area"
##
##  And if Vphi is the velocity in the latitudinal direction
## Then we define Vy = Vphi cos(phi)
##                Kyy = Kphiphi cos^2(phi)
##                Kyz = Kphiz cos(phi) 

###  Should there be more diagnostics defined? E.g. advective and diffusive fluxes?

    # Axis indices for 2D (lat, lev) arrays
    _AXIS_LAT = 0
    _AXIS_LEV = 1

    def __init__(self,
                 Kyy=0.,
                 Kzz=0,
                 Kyz=0,
                 U=0.,
                 W=0.,
                 W_sedimentation=0.,
                 rho=0,  #  Needs documentation. Not currently used.
                 prescribed_flux=0.,  # to be documented
                 interpolation_order=2,  # documented above, currently 1 or 2
                 #  Probably better to have named options like interpolation_order = "linear"
                 use_limiters=True,   # Minimal documentation above, need to clarify what it actually does
                 diagnostic_name_suffix="",  # Should this capability be defined in parent process?
                 **kwargs):
        super(TwoDimensionalAdvectionDiffusion, self).__init__(**kwargs)
        for dom in list(self.domains.values()):
            self._phibounds = np.deg2rad(dom.axes['lat'].bounds)  # radians
            #  Consider renaming "lat" to "y" in the code. This is y = a sin(phi) as defined in Miller (1981)
            self._latbounds = np.sin(self._phibounds) * const.a  # meters
            self._dlatbounds = np.diff(self._latbounds)
            self._levbounds = dom.axes['lev'].bounds *1e2
            self._dlevbounds = np.diff(self._levbounds)
            self._latpoints = 0.5*(self._latbounds[1:] + self._latbounds[:-1])
            self._levpoints = 0.5*(self._levbounds[1:] + self._levbounds[:-1])
        for varname, value in self.state.items():
            self._tracer = value
            self._inner_tracer = self._tracer * 0
            nlat, nlev = value.shape
            self.advective_flux_yy = np.zeros((nlat + 1, nlev))
            self.advective_flux_zz = np.zeros((nlat, nlev + 1))
            self.diffusive_flux_yy = np.zeros((nlat + 1, nlev))
            self.diffusive_flux_zz = np.zeros((nlat, nlev + 1))

        self.prescribed_flux = prescribed_flux  # flux including boundary conditions
        self.interpolation_order = interpolation_order
        self.use_limiters = use_limiters
        self.U = U  # Advecting velocity in units of [length] / [time]
        self.W = W  # Advecting velocity in units of [hpa] / [time]
        self.W_sedimentation = W_sedimentation
        self._Ud = U * 0.0 # Kyz diffusion translated to advective velocities.
        self._Wd = W * 0.0 # Kyz diffusion translated to advective velocities.
        self._Utot = U * 0.0 # total advective velocities (U + Ud)
        self._Wtot = W * 0.0 # total advective velocities (W + Wd)
        self._dt_advdiff = self.timestep_in_seconds
        self.rho = rho
        self.Kyy = Kyy  # Diffusivity in units of [length]**2 / [time]
        self.Kzz = Kzz  # Diffusivity in units of [length]**2 / [time]
        self.Kyz = Kyz
        #  This is mass of air on the staggered grid I think. Probably a clearer way to express this
        self.M_air = 2*np.pi*const.a**2.0*np.diff(np.sin(self._phibounds))[:, None] * self._dlevbounds[None,:] / const.g
        self.diagname_total_tracer_mass = 'total_tracer_mass'+diagnostic_name_suffix
        self.diagname_column_density = 'column_density'+diagnostic_name_suffix
        self.add_diagnostic(self.diagname_total_tracer_mass, np.array([0.0]))
        self.add_diagnostic(self.diagname_column_density, 0.*self._tracer[:,0])
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
        ##  Here the factor cos(phi)**2 is applied
        # The user-facing Kyy is actuall Kphiphi without area weighting
        return np.where(self._Kyy < 0, 0, self._Kyy) * (np.cos(self._phibounds[:, None]))**2.0
    @Kyy.setter
    def Kyy(self, Kvalue):
        self._Kyy = Kvalue

    @property
    def Kzz(self):
        return np.where(self._Kzz < 0, 0, self._Kzz)
    @Kzz.setter
    def Kzz(self, Kvalue):
        self._Kzz = Kvalue

    @property
    def Kyz(self):
        #  Same here, the weighting factor is automatically applied
        return self._Kyz * np.cos(self._phibounds[:, None])
    @Kyz.setter
    def Kyz(self, Kvalue):
        self._Kyz = Kvalue

    @property
    def U(self):
        #  But now I'm confused. Are we applying the area weighting for U elsewhere?
        #  And why is it U and not V?
        return self._U #* np.cos(self._phibounds[:, None])
    @U.setter
    def U(self, Uvalue):
        # Set correct dimensions?
        self._U = Uvalue * np.ones_like(self._phibounds[:, np.newaxis])

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
            tendencies[name] = dtracer / self.timestep_in_seconds

        massmat = self.M_air * self._tracer
        #  Can we use the already computed M here instead??
        area = 2*np.pi*const.a**2.0*np.diff(np.sin(self._phibounds))
        # print(self.diagname_total_tracer_mass)
        # print(type(self.diagname_total_tracer_mass))
        # print(type(massmat))
        # print(type(np.array([np.sum(massmat)])))
        # print(f"total mass: {np.sum(massmat)}")
        self.__setattr__(self.diagname_total_tracer_mass, np.array([np.sum(massmat)]))
        self.__setattr__(self.diagname_column_density, np.sum(massmat, axis=1) / area)
            
        return tendencies

    def _advdiff_timestep(self):
        self._boundary_conditions_tracer()
        self._mixed_diffusion_to_advection()
        self._set_advdiff_dt()
        for j in range(int(self.timestep_in_seconds / self._dt_advdiff)):
            self._advect_along_axis(self._AXIS_LAT)
            self._advect_along_axis(self._AXIS_LEV)
            self._compute_k_fluxes()
            self._update_field_k()

    def _advect_along_axis(self, axis):
        """Perform advection step along specified axis using NIRVANA scheme.

        Uses the numerics module functions which operate on the last array axis.
        Arrays are transposed as needed using np.moveaxis.

        Parameters
        ----------
        axis : int
            Axis to advect along: _AXIS_LAT (0) for latitude, _AXIS_LEV (1) for level
        """
        # Select axis-specific parameters
        if axis == self._AXIS_LAT:
            bounds_1d = self._latbounds
            dbounds_1d = self._dlatbounds
            velocity = self._Utot
            flux_sign = 1.0
            tend_fac_adv = self.tend_fac_fy_adv
            tend_fac_source = self.tend_fac_fy_source_func
            source_func = self._internal_fy_source_func
        else:  # axis == self._AXIS_LEV
            bounds_1d = self._levbounds
            dbounds_1d = self._dlevbounds
            velocity = self._Wtot
            flux_sign = -1.0
            tend_fac_adv = self.tend_fac_fz_adv
            tend_fac_source = self.tend_fac_fz_source_func
            source_func = self._internal_fz_source_func

        # Move target axis to last position for numerics functions
        tracer = np.moveaxis(self._inner_tracer, axis, -1)
        velocity_moved = np.moveaxis(velocity, axis, -1)

        # Create 2D bounds/dbounds arrays by broadcasting 1D arrays
        nlat, nlev = self._inner_tracer.shape
        if axis == self._AXIS_LAT:
            bounds_2d = bounds_1d[:, None] * np.ones((nlat + 1, nlev))
            dbounds_2d = dbounds_1d[:, None] * np.ones((nlat, nlev))
        else:
            bounds_2d = bounds_1d[None, :] * np.ones((nlat, nlev + 1))
            dbounds_2d = dbounds_1d[None, :] * np.ones((nlat, nlev))

        # Move bounds arrays to match tracer orientation
        bounds_2d = np.moveaxis(bounds_2d, axis, -1)
        dbounds_2d = np.moveaxis(dbounds_2d, axis, -1)

        # Compute cumulative integral (operates on last axis)
        integral = numerics.calc_integral(tracer, dbounds_2d)

        # Extend tracer and dbounds for boundary treatment
        tracer_ext = numerics.extend_to_boundaries(tracer, axis=-1)
        dbounds_ext = numerics.extend_dbounds(dbounds_2d, axis=-1)

        # Interpolate integral to upstream position
        if self.interpolation_order == 1:
            istar = numerics.linear_interp_istar(
                bounds_2d, integral, velocity_moved, self._dt_advdiff
            )
        else:  # order == 2
            istar = numerics.parabolic_interp_istar(
                bounds_2d, dbounds_ext, integral, tracer_ext,
                velocity_moved, self._dt_advdiff
            )
            if self.use_limiters:
                istar = numerics.apply_limiters(
                    bounds_2d, integral, istar, velocity_moved, self._dt_advdiff
                )

        # Compute advective flux with appropriate sign convention
        flux = numerics.compute_advective_flux(
            integral, istar, self._dt_advdiff, sign=flux_sign
        )

        # Apply zero-flux boundary conditions
        flux[..., 0] = 0.0
        flux[..., -1] = 0.0

        # Store flux for diagnostics (move back to original axis order)
        flux_original = np.moveaxis(flux, -1, axis)
        if axis == self._AXIS_LAT:
            self.advective_flux_yy = flux_original
        else:
            self.advective_flux_zz = flux_original

        # Compute tracer update from flux convergence
        # For pressure coordinate (lev), need negative sign due to coordinate convention
        if axis == self._AXIS_LAT:
            tracer_update = numerics.apply_flux_convergence(
                flux, dbounds_2d, self._dt_advdiff, tend_fac=tend_fac_adv
            )
        else:
            tracer_update = -numerics.apply_flux_convergence(
                flux, dbounds_2d, self._dt_advdiff, tend_fac=tend_fac_adv
            )

        # Move update back to original axis order and apply
        tracer_update = np.moveaxis(tracer_update, -1, axis)
        self._inner_tracer += tracer_update

        # Apply source function flux contribution
        source_flux = source_func()
        source_flux_moved = np.moveaxis(source_flux, axis, -1)
        if axis == self._AXIS_LAT:
            source_update = numerics.apply_flux_convergence(
                source_flux_moved, dbounds_2d, self._dt_advdiff, tend_fac=tend_fac_source
            )
        else:
            source_update = -numerics.apply_flux_convergence(
                source_flux_moved, dbounds_2d, self._dt_advdiff, tend_fac=tend_fac_source
            )
        source_update = np.moveaxis(source_update, -1, axis)
        self._inner_tracer += source_update

    def _compute_k_fluxes(self):
        """Compute diffusive fluxes in both y and z directions."""
        # Y-direction diffusive flux
        tracer_by = numerics.extend_to_boundaries(self._inner_tracer, axis=0)
        dy_ext = numerics.extend_dbounds(self._dlatbounds)
        dym = 0.5 * (dy_ext[1:] + dy_ext[:-1])
        self.diffusive_flux_yy = -self.Kyy * np.diff(tracer_by, axis=0) / dym[:, None]
        self.diffusive_flux_yy[0, :] = 0.0
        self.diffusive_flux_yy[-1, :] = 0.0

        # Z-direction diffusive flux
        tracer_bz = numerics.extend_to_boundaries(self._inner_tracer, axis=1)
        dz_ext = numerics.extend_dbounds(self._dlevbounds)
        dzm = 0.5 * (dz_ext[1:] + dz_ext[:-1])
        self.diffusive_flux_zz = -self.Kzz * np.diff(tracer_bz, axis=1) / dzm[None, :]
        self.diffusive_flux_zz[:, 0] = 0.0
        self.diffusive_flux_zz[:, -1] = 0.0

    def _mixed_diffusion_to_advection(self):
        """Convert mixed derivative diffusion (Kyz) to effective advective velocities.

        Implements the MALTA scheme from Western et al. (2024) which approximates
        the mixed derivative diffusion terms as effective advective velocities.
        """
        max_tracer = np.max(self._inner_tracer)

        # Extend tracer to boundaries in both directions
        tracer_bz = numerics.extend_to_boundaries(self._inner_tracer, axis=1)
        tracer_by = numerics.extend_to_boundaries(self._inner_tracer, axis=0)

        # Compute effective velocity in y-direction from Kyz * d(chi)/dz
        if np.max(np.abs(self.Kyz)) > 0.0:
            k_y_zmid = 0.5 * (self.Kyz[:, :-1] + self.Kyz[:, 1:])
            tracer_edges = 0.5 * (tracer_bz[:, :-1] + tracer_bz[:, 1:])
            dqdz = np.diff(tracer_edges, axis=1) / self._dlevbounds[None, :]
            dqdz = numerics.extend_to_boundaries(dqdz, axis=0)
            dqdz = 0.5 * (dqdz[1:, :] + dqdz[:-1, :])
            tracer_m = 0.5 * (tracer_by[:-1, :] + tracer_by[1:, :])
            self._Ud = k_y_zmid * dqdz / tracer_m
            self._Ud = np.where(tracer_m < MIXED_DIFFUSION_EPSILON * max_tracer, 0, self._Ud)
        else:
            self._Ud = self._U * 0.0
        self._Utot = self.U + self._Ud

        # Compute effective velocity in z-direction from Kyz * d(chi)/dy
        if np.max(np.abs(self.Kyz)) > 0.0:
            k_ymid_z = 0.5 * (self.Kyz[:-1, :] + self.Kyz[1:, :])
            tracer_edges = 0.5 * (tracer_by[:-1, :] + tracer_by[1:, :])
            dqdy = np.diff(tracer_edges, axis=0) / self._dlatbounds[:, None]
            dqdy = numerics.extend_to_boundaries(dqdy, axis=1)
            dqdy = 0.5 * (dqdy[:, 1:] + dqdy[:, :-1])
            tracer_m = 0.5 * (tracer_bz[:, :-1] + tracer_bz[:, 1:])
            self._Wd = k_ymid_z * dqdy / tracer_m
            self._Wd = np.where(tracer_m < MIXED_DIFFUSION_EPSILON * max_tracer, 0, self._Wd)
        else:
            self._Wd = self._W * 0.0
        self._Wtot = self.W + self._Wd + self.W_sedimentation
        
    def _update_field_k(self):
        """Update tracer field from diffusive fluxes and source terms."""
        self._inner_tracer += self.tend_fac_fy_diff * (self.diffusive_flux_yy[:-1,:] - self.diffusive_flux_yy[1:,:]) * self._dt_advdiff / self._dlatbounds[:, None]
        self._inner_tracer += self.tend_fac_fz_diff * (self.diffusive_flux_zz[:,:-1] - self.diffusive_flux_zz[:,1:]) * self._dt_advdiff / self._dlevbounds[None, :]
        self._inner_tracer += self.tend_fac_source_func * self._internal_source_func() * self._dt_advdiff

    def _boundary_conditions_tracer(self):
        if self.age_of_air == 1:
            self._inner_tracer[self._under_trop] = self.time['days_elapsed'] / 365.0

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
        dt = min(dtlat*fac, dtlev*fac, dtkz*fac, dtky*fac, dtkxy*fac, self.timestep_in_seconds)
        nsteps = np.ceil(self.timestep_in_seconds / dt)
        self._dt_advdiff = self.timestep_in_seconds / nsteps


###  This needs documentation and testing!
###  Maybe also to be moved elsewhere
class ParticleSink(TimeDependentProcess):
    # copied from "ConvectiveAdjustment"
    #Hard Adjustment to a prescribed particle sink.

    def __init__(self, tropopause_p=0, adjust_to_0=True, lifetime=30*86400, **kwargs):
        super(ParticleSink, self).__init__(**kwargs)
        # tropopause definition, in Pa
        self.tropopause_p = tropopause_p
        self.adjust_to_0 = adjust_to_0
        self.lifetime = lifetime
        if(self.adjust_to_0):
            self.time_type = 'adjustment'
            self.adjustment = {}
        for dom in list(self.domains.values()):
            self._levbounds = dom.axes['lev'].bounds *1e2
            self._dlevbounds = np.diff(self._levbounds)
            self._levpoints = 0.5*(self._levbounds[1:] + self._levbounds[:-1])
        self.under_tropopause = self._levpoints[None, :] > self.tropopause_p[:, None]

    @property
    def tropopause_p(self):
        return self._tropopause_p * 1e2
    @tropopause_p.setter
    def tropopause_p(self, tropvalue):
        self._tropopause_p = tropvalue #* 1e2

    def _compute(self):
        self.under_tropopause = self._levpoints[None, :] > self.tropopause_p[:, None]
        if(self.adjust_to_0):
            for name, value in self.state.items():
                if self.tropopause_p is None:
                    self.adjustment[name] = value * 0.
                else:
                    self.adjustment[name] = np.where(self.under_tropopause, -value, 0.0)
            #  return the adjustment, independent of timestep
            #  because the parent process might have set a different timestep!
            return self.adjustment
        else:
            tendencies = {}
            for name, value in self.state.items():
                if self.tropopause_p is None:
                    self.tendencies[name] = 0.0
                else:
                    tendencies[name] = np.where(self.under_tropopause, -value / self.lifetime, 0.0)
            return tendencies            
