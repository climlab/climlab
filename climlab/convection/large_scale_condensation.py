from climlab import constants as const
from climlab.utils.thermo import clausius_clapeyron
from climlab.process.time_dependent_process import TimeDependentProcess
import numpy as np


class LargeScaleCondensation(TimeDependentProcess):
    def __init__(self, 
                 condensation_time = 4*const.seconds_per_hour, 
                 RH_ref = 1.0,
                 **kwargs):
        super(LargeScaleCondensation, self).__init__(**kwargs)
        self.condensation_time = condensation_time
        self.RH_ref = RH_ref
        self.add_diagnostic('latent_heating', 0.*self.Tatm)
        self.add_diagnostic('precipitation_lsc', 0.*self.Ts)
        self.pmin = kwargs.get('pmin', 10.0)
        
    @staticmethod
    def qsat_lsc(T, p):
        """Compute saturation specific humidity as function of temperature and pressure.

        Input:  T is temperature in Kelvin
                p is pressure in hPa or mb
        Output: saturation specific humidity (dimensionless).
        """
        es = clausius_clapeyron(T)
        # qsat = const.eps * es / (p - (1.0 - const.eps) * es )
        qsat = const.eps * es / p # consistent with Frierson's SBM, and is more stable numerically 
        return qsat
    
    def _compute(self):
        qsaturation = self.qsat_lsc(self.Tatm, self.lev) 
        if len(self.Ts) > 1:
            lev_mat, _ = np.meshgrid(self.lev, self.lat)
            has_lat = True
        else:
            lev_mat = self.lev
            has_lat = False
        qtendency = -(self.q - self.RH_ref*qsaturation) / self.condensation_time
        qtendency = np.where(lev_mat<=self.pmin, 0.0, qtendency)

        tendencies = {}
        tendencies['q'] = np.minimum(qtendency, 0.)
        tendencies['Tatm'] = -const.Lhvap/const.cp * tendencies['q']
        self.latent_heating[:] = tendencies['Tatm'] * self.Tatm.domain.heat_capacity
        if has_lat:
            self.precipitation_lsc[:,0] = np.sum(self.latent_heating, axis=-1)/const.Lhvap
        else:
            self.precipitation_lsc[:] = np.sum(self.latent_heating)/const.Lhvap
        return tendencies
