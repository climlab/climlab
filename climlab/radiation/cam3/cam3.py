'''
climlab wrappers for the NCAR CAM3 radiation code

Input arguments and diagnostics follow specifications in
:class:`~climlab.radiation._Radiation`

    :Example:

        Here is a quick example of setting up a single-column
        Radiative-Convective model with fixed relative humdity::

            import climlab
            alb = 0.25
            #  State variables (Air and surface temperature)
            state = climlab.column_state(num_lev=30)
            #  Parent model process
            rcm = climlab.TimeDependentProcess(state=state)
            #  Fixed relative humidity
            h2o = climlab.radiation.ManabeWaterVapor(state=state)
            #  Couple water vapor to radiation
            rad = climlab.radiation.CAM3(state=state, specific_humidity=h2o.q, albedo=alb)
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


'''
from __future__ import division, print_function, absolute_import
import numpy as np
from climlab import constants as const
from climlab.utils.thermo import vmr_to_mmr
from climlab.radiation.radiation import _Radiation_SW, _Radiation_LW
import os, warnings, pooch
import xarray as xr
from climlab.utils import _datapath_http


def init_cam3(mod):
    # Initialise absorptivity / emissivity data
    remotepath_http = _datapath_http + 'absorptivity/abs_ems_factors_fastvx.c030508.nc'
    filehandle = pooch.retrieve(url=remotepath_http,
        known_hash="261043a01b15ebb82ba2baa71311a6807ba9ea6d720baa15bc091f0e61b2a8f2")
    data = xr.open_dataset(filehandle)
    #  Populate storage arrays with values from netcdf file
    for field in ['ah2onw', 'eh2onw', 'ah2ow', 'ln_ah2ow', 'cn_ah2ow', 'ln_eh2ow', 'cn_eh2ow']:
        setattr(mod, field, data[field].transpose())

#  Wrapping these imports in try/except to avoid failures during documentation building on readthedocs
try:
    from . import _cam3
    init_cam3(_cam3.absems)
except Exception:
    warnings.warn('Cannot import and initialize compiled Fortran extension, CAM3 module will not be functional.')


class CAM3(_Radiation_SW, _Radiation_LW):
    '''
    climlab wrapper for the CAM3 radiation code.

    For some details about inputs and diagnostics, see the `radiation` module.
    '''
    def __init__(self,
                 **kwargs):
        super(CAM3, self).__init__(**kwargs)

        self.KM = self.lev.size
        try:
            self.JM = self.lat.size
        except:
            self.JM = 1
        try:
            self.IM = self.lon.size
        except:
            self.IM = 1
        self.do_sw = 1  # '1=do, 0=do not compute SW'
        self.do_lw = 1  # '1=do, 0=do not compute LW'
        self.in_cld = 0 # '1=in-cloud, 0=grid avg cloud water path'

    def _climlab_to_cam3(self, field):
        '''Prepare field with proper dimension order.
        CAM3 code expects 3D arrays with (KM, JM, 1)
        and 2D arrays with (JM, 1).

        climlab grid dimensions are any of:
            - (KM,)
            - (JM, KM)
            - (JM, IM, KM)

        (longitude dimension IM not yet implemented here).'''
        if np.isscalar(field):
            return field
        #  Check to see if column vector needs to be replicated over latitude
        elif self.JM > 1:
            if (field.shape == (self.KM,)):
                return np.tile(field[...,np.newaxis], self.JM)
            else:
                return np.squeeze(np.transpose(field))[..., np.newaxis]
        else:  #  1D vertical model
            return field[..., np.newaxis, np.newaxis]

    def _cam3_to_climlab(self, field):
        ''' Output is either (KM, JM, 1) or (JM, 1).
        Transform this to...
            - (KM,) or (1,)  if JM==1
            - (KM, JM) or (JM, 1)   if JM>1

        (longitude dimension IM not yet implemented).'''
        if self.JM > 1:
            if len(field.shape)==2:
                return field
            elif len(field.shape)==3:
                return np.squeeze(np.transpose(field))
        else:
            return np.squeeze(field)

    def _prepare_arguments(self):
        # scalar integer arguments
        KM = self.KM
        JM = self.JM
        IM = self.IM
        do_sw = self.do_sw
        do_lw = self.do_lw
        in_cld = self.in_cld
        # scalar real arguments
        g = const.g
        Cpd = const.cp
        epsilon = const.Rd / const.Rv
        stebol = const.sigma
        scon = self.S0
        eccf = self.eccentricity_factor
        #  Well-mixed greenhouse gases -- scalar values
        CO2vmr = self.absorber_vmr['CO2']
        N2Ovmr = self.absorber_vmr['N2O']
        CH4vmr = self.absorber_vmr['CH4']
        CFC11vmr = self.absorber_vmr['CFC11']
        CFC12vmr = self.absorber_vmr['CFC12']
        # array input
        Tatm = self._climlab_to_cam3(self.Tatm)
        Ts = self._climlab_to_cam3(self.Ts)
        #insolation = self._climlab_to_cam3(self.insolation * np.ones_like(self.Ts))
        coszen = self._climlab_to_cam3(self.coszen * np.ones_like(self.Ts))
        aldif = self._climlab_to_cam3(self.aldif * np.ones_like(self.Ts))
        aldir = self._climlab_to_cam3(self.aldir * np.ones_like(self.Ts))
        asdif = self._climlab_to_cam3(self.asdif * np.ones_like(self.Ts))
        asdir = self._climlab_to_cam3(self.asdir * np.ones_like(self.Ts))
        #  surface pressure should correspond to model domain!
        ps = self._climlab_to_cam3(self.lev_bounds[-1] * np.ones_like(self.Ts))
        p = self._climlab_to_cam3(self.lev * np.ones_like(self.Tatm))
        #   why are we passing missing instead of the actual layer thicknesses?
        dp = np.zeros_like(p) - 99. # set as missing
        #dp = np.diff(self.lev_bounds)
        #dp = self._climlab_to_cam3(dp * np.ones_like(self.Tatm))
        # Surface upwelling LW
        flus = self._climlab_to_cam3(np.zeros_like(self.Ts) - 99.) # set to missing as default
        # spatially varying gases
        q = self._climlab_to_cam3(self.specific_humidity * np.ones_like(self.Tatm))
        O3vmr = self._climlab_to_cam3(self.absorber_vmr['O3'] * np.ones_like(self.Tatm))
        # convert to mass mixing ratio (needed by CAM3 driver)
        #  The conversion factor is m_o3 / m_air = 48.0 g/mol / 28.97 g/mol
        O3mmr = vmr_to_mmr(O3vmr, gas='O3')
        # cloud fields
        cldfrac = self._climlab_to_cam3(self.cldfrac * np.ones_like(self.Tatm))
        clwp = self._climlab_to_cam3(self.clwp * np.ones_like(self.Tatm))
        ciwp = self._climlab_to_cam3(self.ciwp * np.ones_like(self.Tatm))
        r_liq = self._climlab_to_cam3(self.r_liq * np.ones_like(self.Tatm))
        r_ice = self._climlab_to_cam3(self.r_ice * np.ones_like(self.Tatm))
        #  The ordered list of input fields needed by the CAM3 driver
        args = [KM, JM, IM, do_sw, do_lw, p, dp, ps, Tatm, Ts,
                q, O3mmr, cldfrac, clwp, ciwp, in_cld,
                aldif, aldir, asdif, asdir, eccf, coszen,
                scon, flus, r_liq, r_ice,
                CO2vmr, N2Ovmr, CH4vmr, CFC11vmr,
                CFC12vmr, g, Cpd, epsilon, stebol]
        return args

    def _compute_heating_rates(self):
        # List of arguments to be passed to extension
        args = self._prepare_arguments()
        (TdotRad, SrfRadFlx, qrs, qrl, swflx, swflxc, lwflx, lwflxc, SwToaCf,
            SwSrfCf, LwToaCf, LwSrfCf, LwToa, LwSrf, SwToa, SwSrf,
            swuflx, swdflx, swuflxc, swdflxc,
            lwuflx, lwdflx, lwuflxc, lwdflxc) = _cam3.driver(*args)
        # most of these output fields are unnecessary here
        #  we compute everything from the up and downwelling fluxes
        #  Should probably simplify the fortran wrapper
        #  fluxes at layer interfaces
        self.LW_flux_up = self._cam3_to_climlab(lwuflx) + 0.*self.LW_flux_up
        self.LW_flux_down = self._cam3_to_climlab(lwdflx) + 0.*self.LW_flux_down
        self.LW_flux_up_clr = self._cam3_to_climlab(lwuflxc) + 0.*self.LW_flux_up_clr
        self.LW_flux_down_clr = self._cam3_to_climlab(lwdflxc) + 0.*self.LW_flux_down_clr
        #  fluxes at layer interfaces
        self.SW_flux_up = self._cam3_to_climlab(swuflx) + 0.*self.SW_flux_up
        self.SW_flux_down = self._cam3_to_climlab(swdflx) + 0.*self.SW_flux_down
        self.SW_flux_up_clr = self._cam3_to_climlab(swuflxc) + 0.*self.SW_flux_up_clr
        self.SW_flux_down_clr = self._cam3_to_climlab(swdflxc) + 0.*self.SW_flux_down_clr
        #  Compute quantities derived from fluxes
        self._compute_SW_flux_diagnostics()
        self._compute_LW_flux_diagnostics()
        #  calculate heating rates from flux divergence
        #   this is the total UPWARD flux
        total_flux = self.LW_flux_net - self.SW_flux_net
        LWheating_Wm2 = np.array(np.diff(self.LW_flux_net, axis=-1)) + 0.*self.Tatm
        LWheating_clr_Wm2 = np.array(np.diff(self.LW_flux_net_clr, axis=-1)) + 0.*self.Tatm
        SWheating_Wm2 = np.array(-np.diff(self.SW_flux_net, axis=-1)) + 0.*self.Tatm
        SWheating_clr_Wm2 = np.array(-np.diff(self.SW_flux_net_clr, axis=-1)) + 0.*self.Tatm
        self.heating_rate['Ts'] = np.array(-total_flux[..., -1, np.newaxis]) + 0.*self.Ts
        self.heating_rate['Tatm'] = LWheating_Wm2 + SWheating_Wm2
        #  Convert to K / day
        Catm = self.Tatm.domain.heat_capacity
        self.TdotLW = LWheating_Wm2 / Catm * const.seconds_per_day
        self.TdotLW_clr = LWheating_clr_Wm2 / Catm * const.seconds_per_day
        self.TdotSW = SWheating_Wm2 / Catm * const.seconds_per_day
        self.TdotSW_clr = SWheating_clr_Wm2 / Catm * const.seconds_per_day


class CAM3_LW(CAM3):
    def __init__(self, **kwargs):
        super(CAM3_LW, self).__init__(**kwargs)
        self.do_sw = 0  # '1=do, 0=do not compute SW'
        self.do_lw = 1  # '1=do, 0=do not compute LW'
        #  Albedo needs to be set to 1 currently, otherwise
        #  the CAM code computes solar heating of surface.
        self.asdif = 0.*self.Ts + 1.
        self.asdir = 0.*self.Ts + 1.
        self.aldif = 0.*self.Ts + 1.
        self.aldir = 0.*self.Ts + 1.


class CAM3_SW(CAM3):
    def __init__(self, **kwargs):
        super(CAM3_SW, self).__init__(**kwargs)
        self.do_sw = 1  # '1=do, 0=do not compute SW'
        self.do_lw = 0  # '1=do, 0=do not compute LW'

##  Better to seperate out the LW and SW schemes into seperate Fortran drivers,
## like for RRTM. Among other things, this will help avoid name conflicts.
