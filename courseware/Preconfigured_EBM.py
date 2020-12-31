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
# # Preconfigured Energy Balance Models

# %% [markdown]
# In this document the basic use of climlab's preconfigured EBM class is shown. 
#
# Contents are how to
#
#   * setup an EBM model
#   * show and access subprocesses
#   * integrate the model
#   * access and plot various model variables
#   * calculate the global mean of the temperature

# %%
from __future__ import division, print_function
# %matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
import climlab
from climlab import constants as const

# %% [markdown]
# ### Model Creation

# %% [markdown]
# The regular path for the EBM class is ``climlab.model.ebm.EBM`` but it can also be accessed through ``climlab.EBM``
#
# An EBM model instance is created through

# %%
# model creation
ebm_model = climlab.EBM()

# %% [markdown]
# By default many parameters are set during initialization:
#
# ``num_lat=90, S0=const.S0, A=210., B=2., D=0.55, water_depth=10., Tf=-10, a0=0.3, a2=0.078, ai=0.62, timestep=const.seconds_per_year/90., T0=12., T2=-40``
#
# For further details see the climlab documentation.
#
# Many of the input parameters are stored in the following dictionary:

# %%
# print model parameters
ebm_model.param

# %% [markdown]
# The model consists of one state variable (surface temperature) and a couple of defined subprocesses.

# %%
# print model states and suprocesses
print(ebm_model)

# %% [markdown]
# ### Model subprocesses

# %% [markdown]
# The subprocesses are stored in a dictionary and can be accessed through

# %%
# access model subprocesses
ebm_model.subprocess.keys()

# %% [markdown]
# So to access the time type of the Longwave Radiation subprocess for example, type:

# %%
# access specific subprocess through dictionary
ebm_model.subprocess['LW'].time_type

# %%
#  For interactive convenience, you can also use attribute access for the same thing:
ebm_model.subprocess.LW.time_type

# %% [markdown]
# ### Model integration

# %% [markdown]
# The model time dictionary shows information about all the time related content and quantities.

# %%
# accessing the model time dictionary
ebm_model.time

# %% [markdown]
# To integrate the model forward in time different methods are availible:

# %%
# integrate model for a single timestep
ebm_model.step_forward()

# %% [markdown]
# The model time step has increased from 0 to 1:

# %%
ebm_model.time['steps']

# %%
# integrate model for a 50 days
ebm_model.integrate_days(50.)

# %%
# integrate model for two years
ebm_model.integrate_years(1.)

# %%
# integrate model until solution converges
ebm_model.integrate_converge()

# %% [markdown]
# ## Plotting model variables

# %% [markdown]
# A couple of interesting model variables are stored in a dictionary named ``diagnostics``. It has following entries:

# %%
ebm_model.diagnostics.keys()

# %% [markdown]
# They can be accessed in two ways:
#
# - Through dictionary methods like ``ebm_model.diagnostics['ASR']``
# - As process attributes like ``ebm_model.ASR``

# %%
ebm_model.icelat

# %% [markdown]
# The following code does the plotting for some model variables.

# %%
# creating plot figure
fig = plt.figure(figsize=(15,10))

# Temperature plot
ax1 = fig.add_subplot(221)
ax1.plot(ebm_model.lat,ebm_model.Ts)

ax1.set_xticks([-90,-60,-30,0,30,60,90])
ax1.set_xlim([-90,90])
ax1.set_title('Surface Temperature', fontsize=14)
ax1.set_ylabel('(degC)', fontsize=12)
ax1.grid()

# Albedo plot
ax2 = fig.add_subplot(223, sharex = ax1)
ax2.plot(ebm_model.lat,ebm_model.albedo)

ax2.set_title('Albedo', fontsize=14)
ax2.set_xlabel('latitude', fontsize=10)
ax2.set_ylim([0,1])
ax2.grid()

# Net Radiation plot
ax3 = fig.add_subplot(222, sharex = ax1)
ax3.plot(ebm_model.lat, ebm_model.OLR, label='OLR',
                                       color='cyan')
ax3.plot(ebm_model.lat, ebm_model.ASR, label='ASR',
                                       color='magenta')
ax3.plot(ebm_model.lat, ebm_model.ASR-ebm_model.OLR, 
                                       label='net radiation',
                                       color='red')

ax3.set_title('Net Radiation', fontsize=14)
ax3.set_ylabel('(W/m$^2$)', fontsize=12)
ax3.legend(loc='best')
ax3.grid()

# Energy Balance plot
net_rad = np.squeeze(ebm_model.net_radiation)
transport = ebm_model.heat_transport_convergence()

ax4 = fig.add_subplot(224, sharex = ax1)
ax4.plot(ebm_model.lat, net_rad, label='net radiation', 
                                              color='red')
ax4.plot(ebm_model.lat, transport, label='heat transport', 
                                              color='blue')
ax4.plot(ebm_model.lat, net_rad+transport, label='balance',
                                             color='black')

ax4.set_title('Energy', fontsize=14)
ax4.set_xlabel('latitude', fontsize=10)
ax4.set_ylabel('(W/m$^2$)', fontsize=12)
ax4.legend(loc='best')
ax4.grid()


plt.show()

# %% [markdown]
# The energy balance is zero at every latitude. That means the model is in equilibrium. Perfect!

# %% [markdown]
# ### Global mean temperature

# %% [markdown]
# The model's state dictionary has following entries:

# %%
ebm_model.state.keys()

# %% [markdown]
# Like diagnostics, state variables can be accessed in two ways:
#
# - With dictionary methods, ``ebm_model.state['Ts']`` 
# - As process attributes, ``ebm_model.Ts``
#
# These are entirely equivalent:

# %%
ebm_model.Ts is ebm_model.state['Ts']

# %% [markdown]
# The global mean of the model's surface temperature can be calculated through

# %%
print('The global mean temperature is %.2f deg C.' %climlab.global_mean(ebm_model.Ts))
print('The modeled ice edge is at %.2f deg latitude.' %np.max(ebm_model.icelat))

# %%
