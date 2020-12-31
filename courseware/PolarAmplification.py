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
# #  Polar amplification in simple models

# %%
from __future__ import division, print_function
# %matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
import climlab
from climlab import constants as const

# %% [markdown]
# ##  EBM with surface and atm layers

# %%
ebm = climlab.GreyRadiationModel(num_lev=1, num_lat=90)
insolation = climlab.radiation.AnnualMeanInsolation(domains=ebm.Ts.domain)
ebm.add_subprocess('insolation', insolation)
ebm.subprocess.SW.flux_from_space = ebm.subprocess.insolation.insolation
print(ebm)

# %%
#  add a fixed relative humidity process
#  (will only affect surface evaporation)
h2o = climlab.radiation.ManabeWaterVapor(state=ebm.state, **ebm.param)
ebm.add_subprocess('H2O', h2o)

# %%
#  Add surface heat fluxes
shf = climlab.surface.SensibleHeatFlux(state=ebm.state, Cd=3E-4)
lhf = climlab.surface.LatentHeatFlux(state=ebm.state, Cd=3E-4)
# couple water vapor to latent heat flux process
lhf.q = h2o.q
ebm.add_subprocess('SHF', shf)
ebm.add_subprocess('LHF', lhf)

# %%
ebm.integrate_years(1)

# %%
plt.plot(ebm.lat, ebm.Ts)
plt.plot(ebm.lat, ebm.Tatm)

# %%
co2ebm = climlab.process_like(ebm)
co2ebm.subprocess['LW'].absorptivity = ebm.subprocess['LW'].absorptivity*1.1

# %%
co2ebm.integrate_years(3.)

# %%
#  no heat transport but with evaporation -- no polar amplification
plt.plot(ebm.lat, co2ebm.Ts - ebm.Ts)
plt.plot(ebm.lat, co2ebm.Tatm - ebm.Tatm)

# %% [markdown]
# ### Now with meridional heat transport

# %%
diffebm = climlab.process_like(ebm)
# thermal diffusivity in W/m**2/degC
D = 0.6
# meridional diffusivity in 1/s
K = D / diffebm.Tatm.domain.heat_capacity
d = climlab.dynamics.MeridionalDiffusion(K=K, state={'Tatm': diffebm.Tatm}, **diffebm.param)
diffebm.add_subprocess('diffusion', d)
print(diffebm)

# %%
diffebm.integrate_years(3)

# %%
plt.plot(diffebm.lat, diffebm.Ts)
plt.plot(diffebm.lat, diffebm.Tatm)


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
Rtoa = np.squeeze(diffebm.timeave['ASR'] - diffebm.timeave['OLR'])
plt.plot(diffebm.lat, inferred_heat_transport(Rtoa, diffebm.lat))

# %%
##  Now warm it up!
co2diffebm = climlab.process_like(diffebm)
co2diffebm.subprocess['LW'].absorptivity = diffebm.subprocess['LW'].absorptivity*1.1

# %%
co2diffebm.integrate_years(5)

# %%
#  with heat transport and evaporation 
#   Get some modest polar amplifcation of surface warming
#    but larger equatorial amplification of atmospheric warming
#    Increased atmospheric gradient = increased poleward flux.
plt.plot(diffebm.lat, co2diffebm.Ts - diffebm.Ts, label='Ts')
plt.plot(diffebm.lat, co2diffebm.Tatm - diffebm.Tatm, label='Tatm')
plt.legend()

# %%
Rtoa = np.squeeze(diffebm.timeave['ASR'] - diffebm.timeave['OLR'])
Rtoa_co2 = np.squeeze(co2diffebm.timeave['ASR'] - co2diffebm.timeave['OLR'])
plt.plot(diffebm.lat, inferred_heat_transport(Rtoa, diffebm.lat), label='1xCO2')
plt.plot(diffebm.lat, inferred_heat_transport(Rtoa_co2, diffebm.lat), label='2xCO2')
plt.legend()

# %% [markdown]
# ## Same thing but with NO EVAPORATION

# %%
diffebm2 = climlab.process_like(diffebm)
diffebm2.remove_subprocess('LHF')
diffebm2.integrate_years(3)
co2diffebm2 = climlab.process_like(co2diffebm)
co2diffebm2.remove_subprocess('LHF')
co2diffebm2.integrate_years(3)
#  With transport and no evaporation...
#  No polar amplification, either of surface or air temperature!
plt.plot(diffebm2.lat, co2diffebm2.Ts - diffebm2.Ts, label='Ts')
plt.plot(diffebm2.lat, co2diffebm2.Tatm[:,0] - diffebm2.Tatm[:,0], label='Tatm')
plt.legend()
plt.figure()
#  And in this case, the lack of polar amplification is DESPITE an increase in the poleward heat transport.
Rtoa = np.squeeze(diffebm2.timeave['ASR'] - diffebm2.timeave['OLR'])
Rtoa_co2 = np.squeeze(co2diffebm2.timeave['ASR'] - co2diffebm2.timeave['OLR'])
plt.plot(diffebm2.lat, inferred_heat_transport(Rtoa, diffebm2.lat), label='1xCO2')
plt.plot(diffebm2.lat, inferred_heat_transport(Rtoa_co2, diffebm2.lat), label='2xCO2')
plt.legend()

# %% [markdown]
# ## A column model approach

# %%
model = climlab.GreyRadiationModel(num_lev=30, num_lat=90, abs_coeff=1.6E-4)
insolation = climlab.radiation.AnnualMeanInsolation(domains=model.Ts.domain)
model.add_subprocess('insolation', insolation)
model.subprocess.SW.flux_from_space = model.subprocess.insolation.insolation
print(model)

# %%
#  Convective adjustment for atmosphere only
conv = climlab.convection.ConvectiveAdjustment(state={'Tatm':model.Tatm}, adj_lapse_rate=6.5,
                                                       **model.param)
model.add_subprocess('convective adjustment', conv)

# %%
#  add a fixed relative humidity process
#  (will only affect surface evaporation)
h2o = climlab.radiation.water_vapor.ManabeWaterVapor(state=model.state, **model.param)
model.add_subprocess('H2O', h2o)

# %%
#  Add surface heat fluxes
shf = climlab.surface.SensibleHeatFlux(state=model.state, Cd=1E-3)
lhf = climlab.surface.LatentHeatFlux(state=model.state, Cd=1E-3)
lhf.q = model.subprocess.H2O.q
model.add_subprocess('SHF', shf)
model.add_subprocess('LHF', lhf)

# %%
model.integrate_years(3.)


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
plot_temp_section(model, timeave=False)

# %%
co2model = climlab.process_like(model)
co2model.subprocess['LW'].absorptivity = model.subprocess['LW'].absorptivity*1.1

# %%
co2model.integrate_years(3)

# %%
plot_temp_section(co2model, timeave=False)

# %%
#  Without transport, get equatorial amplification
plt.plot(model.lat, co2model.Ts - model.Ts, label='Ts')
plt.plot(model.lat, co2model.Tatm[:,0] - model.Tatm[:,0], label='Tatm')
plt.legend()

# %% [markdown]
# ##  Now with meridional heat tranpsort!

# %%
diffmodel = climlab.process_like(model)

# %%
# thermal diffusivity in W/m**2/degC
D = 0.05
# meridional diffusivity in 1/s
K = D / diffmodel.Tatm.domain.heat_capacity[0]
print(K)

# %%
d = climlab.dynamics.MeridionalDiffusion(K=K, state={'Tatm':diffmodel.Tatm}, **diffmodel.param)
diffmodel.add_subprocess('diffusion', d)
print(diffmodel)

# %%
diffmodel.integrate_years(3)

# %%
plot_temp_section(diffmodel)

# %%
#  Plot the northward heat transport in this model
Rtoa = np.squeeze(diffmodel.timeave['ASR'] - diffmodel.timeave['OLR'])
plt.plot(diffmodel.lat, inferred_heat_transport(Rtoa, diffmodel.lat))

# %%
##  Now warm it up!
co2diffmodel = climlab.process_like(diffmodel)
co2diffmodel.subprocess['LW'].absorptivity = diffmodel.subprocess['LW'].absorptivity*1.1

# %%
co2diffmodel.integrate_years(3)

# %%
#  With transport, get polar amplification...
#   of surface temperature, but not of air temperature!
plt.plot(diffmodel.lat, co2diffmodel.Ts - diffmodel.Ts, label='Ts')
plt.plot(diffmodel.lat, co2diffmodel.Tatm[:,0] - diffmodel.Tatm[:,0], label='Tatm')
plt.legend()

# %%
Rtoa = np.squeeze(diffmodel.timeave['ASR'] - diffmodel.timeave['OLR'])
Rtoa_co2 = np.squeeze(co2diffmodel.timeave['ASR'] - co2diffmodel.timeave['OLR'])
plt.plot(diffmodel.lat, inferred_heat_transport(Rtoa, diffmodel.lat), label='1xCO2')
plt.plot(diffmodel.lat, inferred_heat_transport(Rtoa_co2, diffmodel.lat), label='2xCO2')

# %% [markdown]
# ## Same thing but with NO EVAPORATION

# %%
diffmodel2 = climlab.process_like(diffmodel)
diffmodel2.remove_subprocess('LHF')
print(diffmodel2)

# %%
diffmodel2.integrate_years(3)

# %%
co2diffmodel2 = climlab.process_like(co2diffmodel)
co2diffmodel2.remove_subprocess('LHF')
co2diffmodel2.integrate_years(3)

# %%
#  With transport and no evaporation...
#  No polar amplification, either of surface or air temperature!
plt.plot(diffmodel2.lat, co2diffmodel2.Ts - diffmodel2.Ts, label='Ts')
plt.plot(diffmodel2.lat, co2diffmodel2.Tatm[:,0] - diffmodel2.Tatm[:,0], label='Tatm')
plt.legend()

# %%
Rtoa = np.squeeze(diffmodel2.timeave['ASR'] - diffmodel2.timeave['OLR'])
Rtoa_co2 = np.squeeze(co2diffmodel2.timeave['ASR'] - co2diffmodel2.timeave['OLR'])
plt.plot(diffmodel2.lat, inferred_heat_transport(Rtoa, diffmodel2.lat), label='1xCO2')
plt.plot(diffmodel2.lat, inferred_heat_transport(Rtoa_co2, diffmodel2.lat), label='2xCO2')

# %% [markdown]
# ## Warming effect of a DECREASE IN EVAPORATION EFFICIENCY
#
# Take a column model that includes evaporation and heat transport, and reduce the drag coefficient by a factor of 2.
#
# How does the surface temperature change?

# %%
diffmodel3 = climlab.process_like(diffmodel)
diffmodel3.subprocess['LHF'].Cd *= 0.5
diffmodel3.integrate_years(5.)

# %%
#  Reduced evaporation gives equatorially enhanced warming of surface
#  and cooling of near-surface air temperature
plt.plot(diffmodel.lat, diffmodel3.Ts - diffmodel.Ts, label='Ts')
plt.plot(diffmodel.lat, diffmodel3.Tatm[:,0] - diffmodel.Tatm[:,0], label='Tatm')
plt.legend()

# %% [markdown]
# ### Same calculation in a two-layer EBM

# %%
diffebm3 = climlab.process_like(diffebm)
diffebm3.subprocess['LHF'].Cd *= 0.5
diffebm3.integrate_years(5.)

# %%
#  Reduced evaporation gives equatorially enhanced warming of surface
#  and cooling of near-surface air temperature
plt.plot(diffebm.lat, diffebm3.Ts - diffebm.Ts, label='Ts')
plt.plot(diffebm.lat, diffebm3.Tatm[:,0] - diffebm.Tatm[:,0], label='Tatm')

# %% [markdown]
# Pretty much the same result.

# %% [markdown]
# ## Some stuff with Band models

# %%
#  Put in some ozone
import netCDF4 as nc

datapath = "http://ramadda.atmos.albany.edu:8080/repository/opendap/latest/Top/Users/BrianRose/CESM_runs/"
endstr = "/entry.das"

ozone = nc.Dataset( datapath + 'som_input/ozone_1.9x2.5_L26_2000clim_c091112.nc' + endstr )

#  Dimensions of the ozone file
lat = ozone.variables['lat'][:]
lon = ozone.variables['lon'][:]
lev = ozone.variables['lev'][:]

# Taking annual, zonal average of the ozone data
O3_zon = np.mean( ozone.variables['O3'],axis=(0,3) )

# %%
#  make a model on the same grid as the ozone
model1 = climlab.BandRCModel(lev=lev, lat=lat)
insolation = climlab.radiation.AnnualMeanInsolation(domains=model1.Ts.domain)
model1.add_subprocess('insolation', insolation)
model1.subprocess.SW.flux_from_space = model1.subprocess.insolation.insolation
print(model1)

# %%
#  Set the ozone mixing ratio
O3_trans = np.transpose(O3_zon)
# Put in the ozone
model1.absorber_vmr['O3'] = O3_trans

# %%
model1.param

# %%
#  Convective adjustment for atmosphere only
model1.remove_subprocess('convective adjustment')
conv = climlab.convection.ConvectiveAdjustment(state={'Tatm':model1.Tatm}, **model1.param)
model1.add_subprocess('convective adjustment', conv)

# %%
#  Add surface heat fluxes
shf = climlab.surface.SensibleHeatFlux(state=model1.state, Cd=0.5E-3)
lhf = climlab.surface.LatentHeatFlux(state=model1.state, Cd=0.5E-3)
#  set the water vapor input field for LHF process
lhf.q = model1.q
model1.add_subprocess('SHF', shf)
model1.add_subprocess('LHF', lhf)

# %%
model1.step_forward()

# %%
model1.integrate_years(1.)

# %%
model1.integrate_years(1.)

# %%
plot_temp_section(model1, timeave=False)

# %%
co2model1 = climlab.process_like(model1)
co2model1.absorber_vmr['CO2'] *= 2

# %%
co2model1.integrate_years(3.)

# %%
plot_temp_section(co2model1, timeave=False)

# %% [markdown]
# Model gets very very hot near equator. Very large equator-to-pole gradient.

# %% [markdown]
# ## Band model with heat transport and evaporation

# %%
diffmodel1 = climlab.process_like(model1)
# thermal diffusivity in W/m**2/degC
D = 0.01
# meridional diffusivity in 1/s
K = D / diffmodel1.Tatm.domain.heat_capacity[0]
d = climlab.dynamics.MeridionalDiffusion(K=K, state={'Tatm': diffmodel1.Tatm}, **diffmodel1.param)
diffmodel1.add_subprocess('diffusion', d)
diffmodel1.absorber_vmr['CO2'] *= 4.
print(diffmodel1)

# %%
diffmodel1.integrate_years(3.)
plot_temp_section(diffmodel1, timeave=False)

# %%
Rtoa = np.squeeze(diffmodel1.timeave['ASR'] - diffmodel1.timeave['OLR'])
plt.plot(diffmodel1.lat, inferred_heat_transport(Rtoa, diffmodel1.lat))

# %%
plt.plot(diffmodel1.lat, diffmodel1.Ts-273.15)

# %%
#  Now double CO2
co2diffmodel1 = climlab.process_like(diffmodel1)
co2diffmodel1.absorber_vmr['CO2'] *= 2.
co2diffmodel1.integrate_years(5)

# %%
#  No polar amplification in this model!
plt.plot(diffmodel1.lat, co2diffmodel1.Ts - diffmodel1.Ts, label='Ts')
plt.plot(diffmodel1.lat, co2diffmodel1.Tatm[:,0] - diffmodel1.Tatm[:,0], label='Tatm')
plt.legend()
plt.figure()
Rtoa = np.squeeze(diffmodel1.timeave['ASR'] - diffmodel1.timeave['OLR'])
Rtoa_co2 = np.squeeze(co2diffmodel1.timeave['ASR'] - co2diffmodel1.timeave['OLR'])
plt.plot(diffmodel1.lat, inferred_heat_transport(Rtoa, diffmodel1.lat), label='1xCO2')
plt.plot(diffmodel1.lat, inferred_heat_transport(Rtoa_co2, diffmodel1.lat), label='2xCO2')
plt.legend()

# %%
