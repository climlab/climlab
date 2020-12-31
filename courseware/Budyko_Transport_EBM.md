---
jupytext:
  formats: ipynb,md:myst,py:percent
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.12
    jupytext_version: 1.8.0
kernelspec:
  display_name: Python 3
  language: python
  name: python3
---

# Budyko Transport for Energy Balance Models

+++

In this document an Energy Balance Model (EBM) is set up with the energy tranport parametrized through the the **budyko type parametrization** term (instead of the default diffusion term), which characterizes the local energy flux through the difference between local temperature and global mean temperature.

+++

$$H(\varphi) = - b [T(\varphi) - \bar{T}]$$

+++

where $T(\varphi)$ is the surface temperature across the latitude $\varphi$, $\bar{T}$ the global mean temperature and $H(\varphi)$ is the transport of energy in an Energy Budget noted as:

+++

$$C(\varphi) \frac{dT(\varphi)}{dt} = R\downarrow (\varphi) - R\uparrow (\varphi) + H(\varphi)$$

```{code-cell} ipython3
from __future__ import division, print_function
import numpy as np
import matplotlib.pyplot as plt
import climlab
from climlab import constants as const
```

### Model Creation

+++

An EBM model instance is created through

```{code-cell} ipython3
# model creation
ebm_budyko = climlab.EBM()
```

The model is set up by default with a meridional diffusion term.

```{code-cell} ipython3
# print model states and suprocesses
print(ebm_budyko)
```

### Create new subprocess

+++

The creation of a subprocess needs some information from the model, especially on which model state the subprocess should be defined on.

```{code-cell} ipython3
# create Budyko subprocess
budyko_transp = climlab.dynamics.BudykoTransport(b=3.81,
                                                 state=ebm_budyko.state,
                                                 **ebm_budyko.param)
```

Note that the model's **whole state dictionary** is given as **input** to the subprocess. In case only the temperature field ``ebm_budyko.state['Ts']`` is given, a new state dictionary would be created which holds the surface temperature with the key ``'default'``. That raises an error as the budyko transport process refers the temperature with key ``'Ts'``.

+++

Now the new transport subprocess has to be merged into the model. The `diffusion` subprocess has to be removed.

```{code-cell} ipython3
# add the new transport subprocess
ebm_budyko.add_subprocess('budyko_transport',budyko_transp)

# remove the old diffusion subprocess
ebm_budyko.remove_subprocess('diffusion')
```

```{code-cell} ipython3
print(ebm_budyko)
```

### Model integration & Plotting

+++

To visualize the model state at beginning of integration we first integrate the model only for one timestep:

```{code-cell} ipython3
# integrate model for a single timestep
ebm_budyko.step_forward()
```

The following code plots the current surface temperature, albedo and energy budget:

```{code-cell} ipython3
# creating plot figure
fig = plt.figure(figsize=(15,10))

# Temperature plot
ax1 = fig.add_subplot(221)
ax1.plot(ebm_budyko.lat,ebm_budyko.Ts)

ax1.set_xticks([-90,-60,-30,0,30,60,90])
ax1.set_xlim([-90,90])
ax1.set_title('Surface Temperature', fontsize=14)
ax1.set_ylabel('(degC)', fontsize=12)
ax1.grid()


# Albedo plot
ax2 = fig.add_subplot(223, sharex = ax1)
ax2.plot(ebm_budyko.lat,ebm_budyko.albedo)

ax2.set_title('Albedo', fontsize=14)
ax2.set_xlabel('latitude', fontsize=10)
ax2.set_ylim([0,1])
ax2.grid()

# Net Radiation plot
ax3 = fig.add_subplot(222, sharex = ax1)
ax3.plot(ebm_budyko.lat, ebm_budyko.OLR, label='OLR',
                                         color='cyan')
ax3.plot(ebm_budyko.lat, ebm_budyko.ASR, label='ASR',
                                         color='magenta')
ax3.plot(ebm_budyko.lat, ebm_budyko.ASR-ebm_budyko.OLR, 
                                         label='net radiation',
                                         color='red')

ax3.set_title('Net Radiation', fontsize=14)
ax3.set_ylabel('(W/m$^2$)', fontsize=12)
ax3.legend(loc='best')
ax3.grid()


# Energy Balance plot
net_rad = ebm_budyko.net_radiation
transport = ebm_budyko.subprocess['budyko_transport'].heating_rate['Ts']

ax4 = fig.add_subplot(224, sharex = ax1)
ax4.plot(ebm_budyko.lat, net_rad, label='net radiation', 
                                             color='red')
ax4.plot(ebm_budyko.lat, transport, label='heat transport', 
                                             color='blue')
ax4.plot(ebm_budyko.lat, net_rad+transport, label='balance',
                                             color='black')

ax4.set_title('Energy', fontsize=14)
ax4.set_xlabel('latitude', fontsize=10)
ax4.set_ylabel('(W/m$^2$)', fontsize=12)
ax4.legend(loc='best')
ax4.grid()


plt.show()
```

The two right sided plots show that the model is not in equilibrium. The net radiation reveals that the model currently gains heat and therefore warms up at the poles and loses heat at the equator. From the Energy plot we can see that latitudinal energy balance is not met.

+++

Now we integrate the model as long there are no more changes in the surface temperature and the model reached equilibrium:

```{code-cell} ipython3
# integrate model until solution converges
ebm_budyko.integrate_converge()
```

```{code-cell} ipython3
# creating plot figure
fig = plt.figure(figsize=(15,10))

# Temperature plot
ax1 = fig.add_subplot(221)
ax1.plot(ebm_budyko.lat,ebm_budyko.Ts)

ax1.set_xticks([-90,-60,-30,0,30,60,90])
ax1.set_xlim([-90,90])
ax1.set_title('Surface Temperature', fontsize=14)
ax1.set_ylabel('(degC)', fontsize=12)
ax1.grid()


# Albedo plot
ax2 = fig.add_subplot(223, sharex = ax1)
ax2.plot(ebm_budyko.lat,ebm_budyko.albedo)

ax2.set_title('Albedo', fontsize=14)
ax2.set_xlabel('latitude', fontsize=10)
ax2.set_ylim([0,1])
ax2.grid()

# Net Radiation plot
ax3 = fig.add_subplot(222, sharex = ax1)
ax3.plot(ebm_budyko.lat, ebm_budyko.OLR, label='OLR',
                                         color='cyan')
ax3.plot(ebm_budyko.lat, ebm_budyko.ASR, label='ASR',
                                         color='magenta')
ax3.plot(ebm_budyko.lat, ebm_budyko.ASR-ebm_budyko.OLR, 
                                         label='net radiation',
                                         color='red')

ax3.set_title('Net Radiation', fontsize=14)
ax3.set_ylabel('(W/m$^2$)', fontsize=12)
ax3.legend(loc='best')
ax3.grid()


# Energy Balance plot
net_rad = ebm_budyko.net_radiation
transport = ebm_budyko.subprocess['budyko_transport'].heating_rate['Ts']

ax4 = fig.add_subplot(224, sharex = ax1)
ax4.plot(ebm_budyko.lat, net_rad, label='net radiation', 
                                             color='red')
ax4.plot(ebm_budyko.lat, transport, label='heat transport', 
                                             color='blue')
ax4.plot(ebm_budyko.lat, net_rad+transport, label='balance',
                                             color='black')

ax4.set_title('Energy', fontsize=14)
ax4.set_xlabel('latitude', fontsize=10)
ax4.set_ylabel('(W/m$^2$)', fontsize=12)
ax4.legend(loc='best')
ax4.grid()


plt.show()
```

Now we can see that the latitudinal energy balance is statisfied. Each latitude gains as much heat (net radiation) as is transported out of it (diffusion transport). There is a net radiation surplus in the equator region, so more shortwave radiation is absorbed there than is emitted through longwave radiation. At the poles there is a net radiation deficit. That imbalance is compensated by the diffusive energy transport term.

+++

### Global mean temperature
We use climlab to compute the global mean temperature and print the ice edge latitude:

```{code-cell} ipython3
print('The global mean temperature is %.2f deg C.' %climlab.global_mean(ebm_budyko.Ts))
print('The modeled ice edge is at %.2f deg latitude.' %np.max(ebm_budyko.icelat))
```

The temperature is a bit too cold for current climate as model parameters are not tuned. Sensitive parameters are ``a0, a2, ai`` and ``Tf`` (albedo), ``A`` and ``B`` (OLR), ``b`` (transport) and ``num_lat`` (grid resolution).

```{code-cell} ipython3

```
