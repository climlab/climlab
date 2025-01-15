import numpy as np
import climlab
import pytest

# A two-dimensional domain
num_lev = 30
num_lat = 60
full_state = climlab.column_state(num_lev=num_lev, num_lat=num_lat, water_depth=10.)
lev = full_state.Tatm.domain.axes['lev'].points
qStrat = 5.E-6
qinitial = 0.*full_state.Tatm + qStrat

short_timestep=climlab.utils.constants.seconds_per_hour
long_timestep = climlab.utils.constants.seconds_per_day

full_state['q'] = qinitial
temp_state = {'Tatm':full_state.Tatm,'Ts':full_state.Ts}
surface_state = {'Ts': full_state.Ts}
atm_state = {'Tatm': full_state['Tatm'], 'q': full_state['q']}

#  Annual mean insolation as a function of latitude and time of year
sun = climlab.radiation.AnnualMeanInsolation(name='Insolation',
                                             domains=full_state['Ts'].domain,
                                             timestep=long_timestep,
                                            )

# thermal diffusivity in W/m**2/degC
D = 0.04
# meridional diffusivity in m**2/s
K = D / atm_state['Tatm'].domain.heat_capacity[0] * climlab.utils.constants.a**2
md = climlab.dynamics.MeridionalDiffusion(name='Moist Diffusion',
                                         state=atm_state,
                                         K=K, 
                                         timestep=long_timestep)

#  Convection scheme -- water vapor is a state variable
conv = climlab.convection.SimplifiedBettsMiller(name='Convection',
                             state=full_state,
                             timestep=short_timestep,  # SBM convection scheme seems to require a short timestep to avoid some instabilities
                            )  
#  Couple the radiation to insolation and water vapor processes
rad = climlab.radiation.RRTMG(name='Radiation',
                              state=temp_state, 
                              specific_humidity=full_state['q'], 
                              albedo=0.125,
                              insolation=sun.insolation,
                              coszen=sun.coszen,
                              timestep=long_timestep,
                              )
lsc = climlab.dynamics.LargeScaleCondensation(name='Large Scale Condensation',
                             state=full_state,
                             timestep =long_timestep
                            )
atm = climlab.couple([rad, conv, md, lsc], name='Atmosphere')

shf = climlab.surface.SensibleHeatFlux(name='SHF',
                                       state=temp_state, 
                                       Cd=0.5E-3, 
                                       timestep=long_timestep,)
lhf = climlab.surface.LatentHeatFlux(name='LHF',
                                     state=full_state, 
                                     Cd=0.5E-3, 
                                     timestep=long_timestep,)


surface = climlab.couple([shf,lhf], name="Slab")

fullmodel = climlab.couple([sun,atm,surface], name='2D Moist Radiative-Convective-Diffusive model')

fullmodel.step_forward()