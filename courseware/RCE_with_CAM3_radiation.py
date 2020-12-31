# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.8.0
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# %% [markdown]
# # Radiative-Convective Equilibrium with CAM3 scheme

# %%
from __future__ import division, print_function
# %matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
import climlab
from climlab import constants as const

# %% [markdown]
# ##  Here is how to set a simple RCE in `climlab`
#
# By initializing each component with the same state object, the components are already effectively coupled. They all act to modify the same state object.
#
# No extra coupling code is necessary.

# %%
# initial state (temperatures)
state = climlab.column_state(num_lev=20, num_lat=1, water_depth=5.)

# %%
#  Create a parent process
rce = climlab.TimeDependentProcess(state=state)
## Create individual physical process models:
#  fixed relative humidity
h2o = climlab.radiation.ManabeWaterVapor(state=state)
#  Hard convective adjustment
convadj = climlab.convection.ConvectiveAdjustment(state=state, adj_lapse_rate=6.5)
# CAM3 radiation with default parameters and interactive water vapor
rad = climlab.radiation.CAM3(state=state, specific_humidity=h2o.q)

# Couple the models
rce.add_subprocess('Radiation', rad)
rce.add_subprocess('ConvectiveAdjustment', convadj)
rce.add_subprocess('H2O', h2o)

# %%
print(rce)

# %%
#  Current state
rce.state

# %%
#  Integrate the model forward
rce.integrate_years(5)

# %%
#  Current state
rce.state

# %%
#  Current specific humidity
rce.q

# %%
#  Here is the dictionary of input fields for the CAM3 radiation module
rce.subprocess.Radiation.input

# %% [markdown]
# ## Latitudinally, seasonally varying RCE

# %%
# initial state (temperatures)
state2 = climlab.column_state(num_lev=20, num_lat=30, water_depth=10.)

# %%
#  Create a parent process
rcelat = climlab.TimeDependentProcess(state=state2)
## Create individual physical process models:
#  seasonal insolation
insol = climlab.radiation.DailyInsolation(domains=rcelat.Ts.domain)
#  fixed relative humidity
h2o = climlab.radiation.ManabeWaterVapor(state=state2)
#  Hard convective adjustment
convadj = climlab.convection.ConvectiveAdjustment(state=state2, adj_lapse_rate=6.5)
# CAM3 radiation with interactive insolation and interactive water vapor
rad = climlab.radiation.CAM3(state=state2, 
                             specific_humidity=h2o.q,
                             S0 = insol.S0,
                             insolation=insol.insolation,
                             coszen=insol.coszen)
# Add all subprocesses to the parent process
rcelat.add_subprocess('Insolation', insol)
rcelat.add_subprocess('Radiation', rad)
rcelat.add_subprocess('ConvectiveAdjustment', convadj)
rcelat.add_subprocess('H2O', h2o)

# %%
rcelat.integrate_years(5)

# %%
rcelat.integrate_years(1)


# %%
def plot_temp_section(model, timeave=True):
    fig = plt.figure()
    ax = fig.add_subplot(111)
    if timeave:
        field = model.timeave['Tatm'].transpose()
    else:
        field = model.Tatm.transpose()
    cax = ax.contourf(model.lat, model.lev, field)
    ax.invert_yaxis()
    ax.set_xlim(-90,90)
    ax.set_xticks([-90, -60, -30, 0, 30, 60, 90])
    fig.colorbar(cax)


# %%
plot_temp_section(rcelat)

# %% [markdown]
# ## Same thing, but also including meridional temperature diffusion

# %%
#  Create and exact clone of the previous model
diffmodel = climlab.process_like(rcelat)

# %%
# thermal diffusivity in W/m**2/degC
D = 0.05
# meridional diffusivity in 1/s
K = D / diffmodel.Tatm.domain.heat_capacity[0]
print(K)

# %%
d = climlab.dynamics.MeridionalDiffusion(K=K, state={'Tatm': diffmodel.Tatm}, **diffmodel.param)

# %%
diffmodel.add_subprocess('diffusion', d)

# %%
diffmodel.integrate_years(5)

# %%
diffmodel.integrate_years(1)

# %%
plot_temp_section(rcelat)
plot_temp_section(diffmodel)


# %%
def inferred_heat_transport( energy_in, lat_deg ):
    '''Returns the inferred heat transport (in PW) by integrating the net energy imbalance from pole to pole.'''
    from scipy import integrate
    from climlab import constants as const
    lat_rad = np.deg2rad( lat_deg )
    return ( 1E-15 * 2 * np.math.pi * const.a**2 * integrate.cumtrapz( np.cos(lat_rad)*energy_in,
            x=lat_rad, initial=0. ) )


# %%
#  Plot the northward heat transport in this model
Rtoa = np.squeeze(diffmodel.timeave['ASR'] - diffmodel.timeave['OLR'])
plt.plot(diffmodel.lat, inferred_heat_transport(Rtoa, diffmodel.lat))

# %% [markdown]
# ## If you want explicit surface fluxes...
#
# All the models above use a convective adjustment that simultaneously adjustments `Tatm` and `Ts` to the prescribed lapse rate.
#
# We can instead limit the convective adjustment to just the atmosphere. To do this, we just have to change the `state` variable dictionary in the convective adjustment process.
#
# Then we can invoke process models for **sensible and latent heat fluxes** that use simple bulk formulae. Tunable parameters for these include drag coefficient and surface wind speed.

# %%
diffmodel2 = climlab.process_like(diffmodel)

#  Hard convective adjustment -- ATMOSPHERE ONLY
convadj2 = climlab.convection.ConvectiveAdjustment(state={'Tatm':diffmodel2.Tatm}, adj_lapse_rate=6.5)
diffmodel2.add_subprocess('ConvectiveAdjustment', convadj2)

print(diffmodel2)

# %%
#  Now add surface flux processes
#  Add surface heat fluxes

shf = climlab.surface.SensibleHeatFlux(state=diffmodel2.state, Cd=0.5E-3)
lhf = climlab.surface.LatentHeatFlux(state=diffmodel2.state, Cd=0.5E-3)
#  set the water vapor input field for LHF process
lhf.q = diffmodel2.subprocess['H2O'].q
diffmodel2.add_subprocess('SHF', shf)
diffmodel2.add_subprocess('LHF', lhf)

print(diffmodel2)

# %%
diffmodel2.integrate_years(5)

# %%
diffmodel2.integrate_years(1)

# %%
plot_temp_section(rcelat)
plot_temp_section(diffmodel2)

# %%
#  Plot the northward heat transport in this model
Rtoa = np.squeeze(diffmodel2.timeave['ASR'] - diffmodel2.timeave['OLR'])
plt.plot(diffmodel2.lat, inferred_heat_transport(Rtoa, diffmodel2.lat))

# %%
