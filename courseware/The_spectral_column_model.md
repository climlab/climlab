---
jupytext:
  notebook_metadata_filter: all,-language_info,-toc,-latex_envs
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

# Modeling spectral bands with `climlab`

+++

Here is a brief introduction to the `climlab.BandRCModel` process.

This is a model that divides the spectrum into 7 distinct bands: three shortwave and four longwave.

As we will see, the process works much like the familiar `climlab.RadiativeConvectiveModel`.

+++

## About the spectra

+++

The shortwave is divided into three channels:

- Channel 0 is the Hartley and Huggins band (extreme UV, 200 - 340 nm, 1% of total flux, strong ozone absorption)
- Channel 1 is Chappuis band (450 - 800 nm, 27% of total flux, moderate ozone absorption)
- Channel 2 is remaining radiation (72% of total flux, largely in the visible range, no ozone absorption)

The longwave is divided into four bands:

- Band 0 is the window region (between 8.5 and 11 $\mu$m), 17% of total flux.
- Band 1 is the CO2 absorption channel (the band of strong absorption by CO2 around 15 $\mu$m), 15% of total flux
- Band 2 is a weak water vapor absorption channel, 35% of total flux
- Band 3 is a strong water vapor absorption channel, 33% of total flux

The longwave decomposition is not as easily related to specific wavelengths, as in reality there is a lot of overlap between H$_2$O and CO$_2$ absorption features (as well as absorption by other greenhouse gases such as CH$_4$ and N$_2$O that we are not representing).

+++

### Example usage of the spectral model

```{code-cell} ipython3
from __future__ import division, print_function
%matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
import climlab
from climlab import constants as const
```

First try a model with all default parameters. Usage is very similar to the familiar `RadiativeConvectiveModel`.

```{code-cell} ipython3
col1 = climlab.BandRCModel()
print(col1)
```

Check out the list of subprocesses.

We now have a process called `H2O`, in addition to things we've seen before.

This model keeps track of water vapor. We see the specific humidity in the list of state variables:

```{code-cell} ipython3
col1.state
```

The water vapor field is initialized to zero. The `H2O` process will set the specific humidity field at every timestep to a specified profile. More on that below. For now, let's compute a radiative equilibrium state.

```{code-cell} ipython3
col1.integrate_years(2)
```

```{code-cell} ipython3
# Check for energy balance
col1.ASR - col1.OLR
```

```{code-cell} ipython3
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot( col1.Tatm, col1.lev, 'c-', label='default' )
ax.plot( col1.Ts, climlab.constants.ps, 'co', markersize=16 )
ax.invert_yaxis()
ax.set_xlabel('Temperature (K)', fontsize=16)
ax.set_ylabel('Pressure (hPa)', fontsize=16 )
ax.set_title('Temperature profiles', fontsize = 18)
ax.grid()
```

By default this model has convective adjustment.  We can set the adjusted lapse rate by passing a parameter when we create the model.

The model currently has no ozone (so there is no stratosphere). Not very realistic!

+++

More reasonable-looking troposphere, but still no stratosphere.

+++

### About the radiatively active gases

+++

The Band model is aware of three different absorbing gases: O3 (ozone), CO2, and H2O (water vapor). The abundances of these gases are stored in a dictionary of arrays as follows:

```{code-cell} ipython3
col1.absorber_vmr
```

Ozone and CO2 are both specified in the model. The default, as you see above, is zero ozone, and constant (well-mixed) CO2 at a volume mixing ratio of 3.8E-4 or 380 ppm.

Water vapor is handled differently: it is determined by the model at each timestep. We make the following assumptions, following a classic paper on radiative-convective equilibrium by Manabe and Wetherald (J. Atmos. Sci. 1967):

- the relative humidity just above the surface is fixed at 77% (can be changed of course... see the parameter `col1.relative_humidity`
- water vapor drops off linearly with pressure
- there is a small specified amount of water vapor in the stratosphere.

+++

## Putting in some ozone

+++

We need to provide some ozone data to the model in order to simulate a stratosphere. As we did with the original column model, we will use the ozone climatology data provided with the CESM model.

See here for more information, including some plots of the ozone data:
<http://www.atmos.albany.edu/facstaff/brose/classes/ENV480_Spring2014/styled-5/code-3/index.html>

```{code-cell} ipython3
import netCDF4 as nc
from pathlib import Path

datapath = Path() / "ozone_1.9x2.5_L26_2000clim_c091112.nc"
ozone = nc.Dataset(datapath)
```

```{code-cell} ipython3
#  Dimensions of the ozone file
lat = ozone.variables['lat'][:]
lon = ozone.variables['lon'][:]
lev = ozone.variables['lev'][:]
```

```{code-cell} ipython3
# Taking annual, zonal, and global averages of the ozone data
O3_zon = np.mean( ozone.variables['O3'],axis=(0,3) )
O3_global = np.sum( O3_zon * np.cos(np.deg2rad(lat)), axis=1 ) / sum( np.cos(np.deg2rad(lat) ) )
```

```{code-cell} ipython3
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot( O3_global*1E6, lev)
ax.invert_yaxis()
```

We are going to create another instance of the model, this time using the same vertical coordinates as the ozone data.

```{code-cell} ipython3
#  Create the column with appropriate vertical coordinate, surface albedo and convective adjustment
col2 = climlab.BandRCModel(lev=lev)
print(col2)
```

```{code-cell} ipython3
#  Set the ozone mixing ratio
col2.absorber_vmr['O3'] = O3_global
```

```{code-cell} ipython3
#  Run the model out to equilibrium!
col2.integrate_years(2.)
```

```{code-cell} ipython3
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot( col1.Tatm, np.log(col1.lev/1000), 'c-', label='RCE' )
ax.plot( col1.Ts, 0, 'co', markersize=16 )
ax.plot(col2.Tatm, np.log(col2.lev/1000), 'r-', label='RCE O3' )
ax.plot(col2.Ts, 0, 'ro', markersize=16 )
ax.invert_yaxis()
ax.set_xlabel('Temperature (K)', fontsize=16)
ax.set_ylabel('log(Pressure)', fontsize=16 )
ax.set_title('Temperature profiles', fontsize = 18)
ax.grid()
ax.legend()
```

Once we include ozone we get a well-defined stratosphere. We can also a slight cooling effect in the troposphere.

Things to consider / try:

- Here we used the global annual mean Q = 341.3 W m$^{-2}$. We might want to consider latitudinal or seasonal variations in Q.
- We also used the global annual mean ozone profile! Ozone varies tremendously in latitude and by season. That information is all contained in the ozone data file we opened above. We might explore the effects of those variations.
- We can calculate climate sensitivity in this model by doubling the CO2 concentration and re-running out to the new equilibrium. Does the amount of ozone affect the climate sensitivity?  (example below)
- An important shortcoming of the model: there are no clouds! (that would be the next step in the hierarchy of column models)
- Clouds would act both in the shortwave (increasing the albedo, cooling the climate) and in the longwave (greenhouse effect, warming the climate). Which effect is stronger depends on the vertical structure of the clouds (high or low clouds) and their optical properties (e.g. thin cirrus clouds are nearly transparent to solar radiation but are good longwave absorbers).

```{code-cell} ipython3
col3 = climlab.process_like(col2)
print(col3)
```

```{code-cell} ipython3
# Let's double CO2.
col3.absorber_vmr['CO2'] *= 2.
```

```{code-cell} ipython3
col3.compute_diagnostics()
print('The radiative forcing for doubling CO2 is %f W/m2.' % (col2.OLR - col3.OLR))
```

```{code-cell} ipython3
col3.integrate_years(3)
```

```{code-cell} ipython3
col3.ASR - col3.OLR
```

```{code-cell} ipython3
print('The Equilibrium Climate Sensitivity is %f K.' % (col3.Ts - col2.Ts))
```

```{code-cell} ipython3
col4 = climlab.process_like(col1)
print(col4)
```

```{code-cell} ipython3
col4.absorber_vmr
```

```{code-cell} ipython3
col4.absorber_vmr['CO2'] *= 2.
col4.compute_diagnostics()
print('The radiative forcing for doubling CO2 is %f W/m2.' % (col1.OLR - col4.OLR))
```

```{code-cell} ipython3
col4.integrate_years(3.)
col4.ASR - col4.OLR
```

```{code-cell} ipython3
print('The Equilibrium Climate Sensitivity is %f K.' % (col4.Ts - col1.Ts))
```

Interesting that the model is MORE sensitive when ozone is set to zero.

```{code-cell} ipython3

```
