'''
A climlab process for the Emanuel convection scheme
'''
from __future__ import absolute_import

import numpy as np
import warnings
from climlab.process import TimeDependentProcess
from climlab.utils.thermo import qsat
from climlab import constants as const
try:
    from ._emanuel_convection import emanuel_convection as convect
except:
    warnings.warn('Cannot import EmanuelConvection fortran extension, this module will not be functional.')
# The array conversion routines we are borrowing from the RRTMG wrapper
from climlab.radiation.rrtm.utils import _climlab_to_rrtm as _climlab_to_convect
from climlab.radiation.rrtm.utils import _rrtm_to_climlab as _convect_to_climlab


# Thermodynamic constants
CPD = const.cp
CPV = const.cpv
RV = const.Rv
RD = const.Rd
LV0 = const.Lhvap
G = const.g
ROWL = const.rho_w
# specific heat of liquid water -- artifically small!
#   Kerry Emanuel's notes say this is intentional, do not change this.
CL=2500.0
#CPV = CPD  #  try neglecting effect of water vapor on heat capacity

class EmanuelConvection(TimeDependentProcess):
    '''
    The climlab wrapper for Kerry Emanuel's moist convection scheme <https://emanuel.mit.edu/FORTRAN-subroutine-convect>

    From the documentation distributed with the Fortran 77 code CONVECT:

        The subroutine is designed to be used in time-marching models of mesoscale to global-scale dimensions.
        It is meant to represent the effects of all moist convection, including shallow, non-precipitating cumulus.
        It also contains a dry adiabatic adjustment scheme.

        Since the method of calculating the convective fluxes involves a relaxation toward quasi-equilibrium,
        subroutine CONVECT must be run for at least several time steps to give meaningful results.
        At the first time step, the tendencies and convective precipitation will be zero.
        If the initial sounding is unstable, these will rapidly increase over successive time steps,
        depending on the values of the constants ALPHA and DAMP.
        Thus the user interested in convective fluxes and precipitation
        associated with a single initial sounding (i.e., without large-scale forcing)
        should still march CONVECT forward enough time steps that the fluxes have
        returned back to zero;
        the net tendencies and precipitation integrated over this time interval are then the desired results.
        But it should be cautioned that these quantities will not necessarily be
        independent of other model parameters such as the time step.
        CONVECT is very much built on the philosophy that convection,
        to the extent it can be represented in terms of large-scale variables,
        is never very far away from statistical equilibrium with the large-scale flow.
        To achieve a smooth evolution of the convective forcing,
        CONVECT should be called at least every 20 minutes during the time integration.
        CONVECT will work at longer time intervals, but the convective tendencies may become noisy.

    Basic characteristics:

    State:

        - Ts (surface radiative temperature -- optional, and ignored)
        - Tatm (air temperature in K)
        - q (specific humidity in kg/kg)
        - U (zonal velocity in m/s -- optional)
        - V (meridional velocity in m/s -- optional)

    Input arguments and default values (taken from convect43.f fortran source):

        - MINORIG = 0,  index of lowest level from which convection may originate (zero means lowest)
        - ELCRIT = 0.0011,  autoconversion threshold water content (g/g)
        - TLCRIT = -55.0, critical temperature below which the auto-conversion threshold is assumed to be zero (the autoconversion threshold varies linearly between 0 C and TLCRIT)
        - ENTP = 1.5, coefficient of mixing in the entrainment formulation
        - SIGD = 0.05, fractional area covered by unsaturated downdraft
        - SIGS = 0.12, fraction of precipitation falling outside of cloud
        - OMTRAIN = 50.0, assumed fall speed (Pa/s) of rain
        - OMTSNOW = 5.5, assumed fall speed (Pa/s) of snow
        - COEFFR = 1.0, coefficient governing the rate of evaporation of rain
        - COEFFS = 0.8, coefficient governing the rate of evaporation of snow
        - CU = 0.7, coefficient governing convective momentum transport
        - BETA = 10.0, coefficient used in downdraft velocity scale calculation
        - DTMAX = 0.9, maximum negative temperature perturbation a lifted parcel is allowed to have below its LFC
        - ALPHA = 0.2, first parameter that controls the rate of approach to quasi-equilibrium
        - DAMP = 0.1, second parameter that controls the rate of approach to quasi-equilibrium (DAMP must be less than 1)
        - IPBL = 0, switch to bypass the dry convective adjustment (bypass if IPBL==0)

    Tendencies computed:

        - air temperature (K/s)
        - specific humidity (kg/kg/s)
        - optional:
            - U and V wind components (m/s/s), if U and V are included in state dictionary

    Diagnostics computed:

        - CBMF (cloud base mass flux in kg/m2/s) -- this is actually stored internally and used as input for subsequent timesteps
        - precipitation (convective precipitation rate in kg/m2/s or mm/s)
        - relative_humidity (dimensionless)

        :Example:

            Here is an example of setting up a single-column
            Radiative-Convective model with interactive water vapor.

            This example also demonstrates *asynchronous coupling*:
            the radiation uses a longer timestep than the other model components::

                import numpy as np
                import climlab
                from climlab import constants as const
                # Temperatures in a single column
                full_state = climlab.column_state(num_lev=30, water_depth=2.5)
                temperature_state = {'Tatm':full_state.Tatm,'Ts':full_state.Ts}
                #  Initialize a nearly dry column (small background stratospheric humidity)
                q = np.ones_like(full_state.Tatm) * 5.E-6
                #  Add specific_humidity to the state dictionary
                full_state['q'] = q
                #  ASYNCHRONOUS COUPLING -- the radiation uses a much longer timestep
                #  The top-level model
                model = climlab.TimeDependentProcess(state=full_state,
                                              timestep=const.seconds_per_hour)
                #  Radiation coupled to water vapor
                rad = climlab.radiation.RRTMG(state=temperature_state,
                                              specific_humidity=full_state.q,
                                              albedo=0.3,
                                              timestep=const.seconds_per_day
                                              )
                #  Convection scheme -- water vapor is a state variable
                conv = climlab.convection.EmanuelConvection(state=full_state,
                                              timestep=const.seconds_per_hour)
                #  Surface heat flux processes
                shf = climlab.surface.SensibleHeatFlux(state=temperature_state, Cd=0.5E-3,
                                              timestep=const.seconds_per_hour)
                lhf = climlab.surface.LatentHeatFlux(state=full_state, Cd=0.5E-3,
                                              timestep=const.seconds_per_hour)
                #  Couple all the submodels together
                model.add_subprocess('Radiation', rad)
                model.add_subprocess('Convection', conv)
                model.add_subprocess('SHF', shf)
                model.add_subprocess('LHF', lhf)
                print(model)

                #  Run the model
                model.integrate_years(1)
                #  Check for energy balance
                print(model.ASR - model.OLR)
    '''
    def __init__(self,
            MINORIG = 0,  # index of lowest level from which convection may originate (zero means lowest)
            #  Default parameter values taken from convect43c.f  fortran source
            ELCRIT=.0011,
            TLCRIT=-55.0,
            ENTP=1.5,
            SIGD=0.05,
            SIGS=0.12,
            OMTRAIN=50.0,
            OMTSNOW=5.5,
            COEFFR=1.0,
            COEFFS=0.8,
            CU=0.7,
            BETA=10.0,
            DTMAX=0.9,
            ALPHA=0.2,
            DAMP=0.1,
            IPBL=0,
            **kwargs):
        super(EmanuelConvection, self).__init__(**kwargs)
        self.time_type = 'explicit'
        #  Define inputs and diagnostics
        surface_shape = self.state['Tatm'][...,0].shape
        #  Hack to handle single column and multicolumn
        if surface_shape == ():
            init = np.atleast_1d(np.zeros(surface_shape))
            self.multidim=False
        else:
            init = np.zeros(surface_shape)[...,np.newaxis]
            self.multidim=True
        self.add_diagnostic('CBMF', init*0.)  # cloud base mass flux
        self.add_diagnostic('precipitation', init*0.) # Precip rate (kg/m2/s)
        self.add_diagnostic('relative_humidity', 0*self.Tatm)
        self.add_input('MINORIG', MINORIG)
        self.add_input('ELCRIT', ELCRIT)
        self.add_input('TLCRIT', TLCRIT)
        self.add_input('ENTP', ENTP)
        self.add_input('SIGD', SIGD)
        self.add_input('SIGS', SIGS)
        self.add_input('OMTRAIN', OMTRAIN)
        self.add_input('OMTSNOW', OMTSNOW)
        self.add_input('COEFFR', COEFFR)
        self.add_input('COEFFS', COEFFS)
        self.add_input('CU', CU)
        self.add_input('BETA', BETA)
        self.add_input('DTMAX', DTMAX)
        self.add_input('ALPHA', ALPHA)
        self.add_input('DAMP', DAMP)
        self.add_input('IPBL', IPBL)

    def _compute(self):
        #  Invert arrays so the first element is the bottom of column
        T = _climlab_to_convect(self.state['Tatm'])
        dom = self.state['Tatm'].domain
        P = _climlab_to_convect(dom.lev.points)
        PH = _climlab_to_convect(dom.lev.bounds)
        Q = _climlab_to_convect(self.state['q'])
        QS = qsat(T,P)
        ND = np.size(T, axis=1)
        NCOL = np.size(T, axis=0)
        NL = ND-1
        try:
            U = _climlab_to_convect(self.state['U'])
        except:
            U = np.zeros_like(T)
        try:
            V = _climlab_to_convect(self.state['V'])
        except:
            V = np.zeros_like(T)
        NTRA = 1
        TRA = np.zeros((NCOL,ND,NTRA), order='F')  # tracers ignored
        DELT = float(self.timestep)
        CBMF = self.CBMF
        (IFLAG, FT, FQ, FU, FV, FTRA, PRECIP, WD, TPRIME, QPRIME, CBMFnew,
        Tout, Qout, QSout, Uout, Vout, TRAout) = \
            convect(T, Q, QS, U, V, TRA, P, PH, NCOL, ND, NL, NTRA, DELT, self.IPBL, CBMF,
                    CPD, CPV, CL, RV, RD, LV0, G, ROWL, self.MINORIG,
                    self.ELCRIT, self.TLCRIT, self.ENTP, self.SIGD, self.SIGS,
                    self.OMTRAIN, self.OMTSNOW, self.COEFFR, self.COEFFS,
                    self.CU, self.BETA, self.DTMAX, self.ALPHA, self.DAMP
                    )
        # If dry adjustment is being used then the tendencies need to be adjusted
        if self.IPBL != 0:
            FT += (Tout - T) / DELT
            FQ += (Qout - Q) / DELT
        tendencies = {'Tatm': _convect_to_climlab(FT)*np.ones_like(self.state['Tatm']),
                      'q': _convect_to_climlab(FQ)*np.ones_like(self.state['q'])}
        if 'Ts' in self.state:
            # for some strange reason self.Ts is breaking tests under Python 3.5 in some configurations
            tendencies['Ts'] = 0. * self.state['Ts']
        if 'U' in self.state:
            tendencies['U'] = _convect_to_climlab(FU) * np.ones_like(self.state['U'])
        if 'V' in self.state:
            tendencies['V'] = _convect_to_climlab(FV) * np.ones_like(self.state['V'])
        self.CBMF = CBMFnew
        #  Need to convert from mm/day to mm/s or kg/m2/s
        #  Hack to handle single column and multicolumn
        if self.multidim:
            self.precipitation[:,0] = _convect_to_climlab(PRECIP)/const.seconds_per_day
        else:
            self.precipitation[:] = _convect_to_climlab(PRECIP)/const.seconds_per_day
        self.IFLAG = IFLAG
        self.relative_humidity[:] = self.q / qsat(self.Tatm,self.lev)
        return tendencies
