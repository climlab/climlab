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

# Comparing soundings from NCEP Reanalysis and various models

+++

We are going to plot the global, annual mean sounding (vertical temperature profile) from observations.

Read in the necessary NCEP reanalysis data from the online server.

The catalog is here: <http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis.derived/catalog.html>

```{code-cell}
from __future__ import division, print_function
import numpy as np
import matplotlib.pyplot as plt
import netCDF4 as nc

ncep_url = "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis.derived/"
ncep_air = nc.Dataset( ncep_url + "pressure/air.mon.1981-2010.ltm.nc" )
level = ncep_air.variables['level'][:]
lat = ncep_air.variables['lat'][:]
```

Take global averages and time averages.

```{code-cell}
Tzon = np.mean(ncep_air.variables['air'],axis=(0,3))
Tglobal = np.sum( Tzon * np.cos(np.deg2rad(lat)), axis=1 ) / np.sum( np.cos(np.deg2rad(lat) ) )
```

Here is code to make a nicely labeled sounding plot.

```{code-cell}
fig = plt.figure( figsize=(10,8) )
ax = fig.add_subplot(111)
ax.plot( Tglobal + 273.15, np.log(level/1000) )
ax.invert_yaxis()
ax.set_xlabel('Temperature (K)', fontsize=16)
ax.set_ylabel('Pressure (hPa)', fontsize=16 )
ax.set_yticks( np.log(level/1000) )
ax.set_yticklabels( level )
ax.set_title('Global, annual mean sounding from NCEP Reanalysis', fontsize = 24)
ax2 = ax.twinx()
ax2.plot( Tglobal + 273.15, -8*np.log(level/1000) );
ax2.set_ylabel('Approx. height above surface (km)', fontsize=16 );
ax.grid()
```

## Now compute the Radiative Equilibrium solution for the grey-gas column model

```{code-cell}
import climlab
from climlab import constants as const
```

```{code-cell}
col = climlab.GreyRadiationModel()
print(col)
```

```{code-cell}
col.subprocess['LW'].diagnostics
```

```{code-cell}
col.integrate_years(1)

print("Surface temperature is " + str(col.Ts) + " K.")
print("Net energy in to the column is " + str(col.ASR - col.OLR) + " W / m2.")
```

### Plot the radiative equilibrium temperature on the same plot with NCEP reanalysis

```{code-cell}
pcol = col.lev

fig = plt.figure( figsize=(10,8) )
ax = fig.add_subplot(111)
ax.plot( Tglobal + 273.15, np.log(level/1000), 'b-', col.Tatm, np.log( pcol/const.ps ), 'r-' )
ax.plot( col.Ts, 0, 'ro', markersize=20 )
ax.invert_yaxis()
ax.set_xlabel('Temperature (K)', fontsize=16)
ax.set_ylabel('Pressure (hPa)', fontsize=16 )
ax.set_yticks( np.log(level/1000) )
ax.set_yticklabels( level )
ax.set_title('Temperature profiles: observed (blue) and radiative equilibrium in grey gas model (red)', fontsize = 18)
ax2 = ax.twinx()
ax2.plot( Tglobal + const.tempCtoK, -8*np.log(level/1000) );
ax2.set_ylabel('Approx. height above surface (km)', fontsize=16 );
ax.grid()
```

## Now use convective adjustment to compute a Radiative-Convective Equilibrium temperature profile

```{code-cell}
dalr_col = climlab.RadiativeConvectiveModel(adj_lapse_rate='DALR')
print(dalr_col)
```

```{code-cell}
dalr_col.integrate_years(2.)

print("After " + str(dalr_col.time['days_elapsed']) + " days of integration:")
print("Surface temperature is " + str(dalr_col.Ts) + " K.")
print("Net energy in to the column is " + str(dalr_col.ASR - dalr_col.OLR) + " W / m2.")
```

```{code-cell}
dalr_col.param
```

Now plot this "Radiative-Convective Equilibrium" on the same graph:

```{code-cell}
fig = plt.figure( figsize=(10,8) )
ax = fig.add_subplot(111)
ax.plot( Tglobal + 273.15, np.log(level/1000), 'b-', col.Tatm, np.log( pcol/const.ps ), 'r-' )
ax.plot( col.Ts, 0, 'ro', markersize=16 )
ax.plot( dalr_col.Tatm, np.log( pcol / const.ps ), 'k-' )
ax.plot( dalr_col.Ts, 0, 'ko', markersize=16 )
ax.invert_yaxis()
ax.set_xlabel('Temperature (K)', fontsize=16)
ax.set_ylabel('Pressure (hPa)', fontsize=16 )
ax.set_yticks( np.log(level/1000) )
ax.set_yticklabels( level )
ax.set_title('Temperature profiles: observed (blue), RE (red) and dry RCE (black)', fontsize = 18)
ax2 = ax.twinx()
ax2.plot( Tglobal + const.tempCtoK, -8*np.log(level/1000) );
ax2.set_ylabel('Approx. height above surface (km)', fontsize=16 );
ax.grid()
```

The convective adjustment gets rid of the unphysical temperature difference between the surface and the overlying air.

But now the surface is colder! Convection acts to move heat upward, away from the surface.

Also, we note that the observed lapse rate (blue) is always shallower than $\Gamma_d$ (temperatures decrease more slowly with height).

+++

## "Moist" Convective Adjustment

+++

To approximately account for the effects of latent heat release in rising air parcels, we can just adjust to a lapse rate that is a little shallow than $\Gamma_d$.

We will choose 6 K / km, which gets close to the observed mean lapse rate.

We will also re-tune the longwave absorptivity of the column to get a realistic surface temperature of 288 K:

```{code-cell}
rce_col = climlab.RadiativeConvectiveModel(adj_lapse_rate=6, abs_coeff=1.7E-4)
print(rce_col)
```

```{code-cell}
rce_col.integrate_years(2.)

print("After " + str(rce_col.time['days_elapsed']) + " days of integration:")
print("Surface temperature is " + str(rce_col.Ts) + " K.")
print("Net energy in to the column is " + str(rce_col.ASR - rce_col.OLR) + " W / m2.")
```

Now add this new temperature profile to the graph:

```{code-cell}
fig = plt.figure( figsize=(10,8) )
ax = fig.add_subplot(111)
ax.plot( Tglobal + 273.15, np.log(level/1000), 'b-', col.Tatm, np.log( pcol/const.ps ), 'r-' )
ax.plot( col.Ts, 0, 'ro', markersize=16 )
ax.plot( dalr_col.Tatm, np.log( pcol / const.ps ), 'k-' )
ax.plot( dalr_col.Ts, 0, 'ko', markersize=16 )
ax.plot( rce_col.Tatm, np.log( pcol / const.ps ), 'm-' )
ax.plot( rce_col.Ts, 0, 'mo', markersize=16 )
ax.invert_yaxis()
ax.set_xlabel('Temperature (K)', fontsize=16)
ax.set_ylabel('Pressure (hPa)', fontsize=16 )
ax.set_yticks( np.log(level/1000) )
ax.set_yticklabels( level )
ax.set_title('Temperature profiles: observed (blue), RE (red), dry RCE (black), and moist RCE (magenta)', fontsize = 18)
ax2 = ax.twinx()
ax2.plot( Tglobal + const.tempCtoK, -8*np.log(level/1000) );
ax2.set_ylabel('Approx. height above surface (km)', fontsize=16 );
ax.grid()
```

## Adding stratospheric ozone

+++

Our model has no equivalent of the stratosphere, where temperature increases with height. That's because our model has been completely transparent to shortwave radiation up until now.

We can load the observed ozone climatology from the input files for the CESM model:

```{code-cell}
datapath = "http://ramadda.atmos.albany.edu:8080/repository/opendap/latest/Top/Users/BrianRose/CESM_runs/"
endstr = "/entry.das"
ozone = nc.Dataset( datapath + 'som_input/ozone_1.9x2.5_L26_2000clim_c091112.nc' + endstr )
```

```{code-cell}
print(ozone.variables['O3'])
```

```{code-cell}
lat = ozone.variables['lat'][:]
lon = ozone.variables['lon'][:]
lev = ozone.variables['lev'][:]
```

The pressure levels in this dataset are:

```{code-cell}
print(lev)
```

Take the global average of the ozone climatology, and plot it as a function of pressure (or height)

```{code-cell}
O3_zon = np.mean( ozone.variables['O3'],axis=(0,3) )
O3_global = np.sum( O3_zon * np.cos(np.deg2rad(lat)), axis=1 ) / np.sum( np.cos(np.deg2rad(lat) ) )
```

```{code-cell}
O3_global.shape
```

```{code-cell}
ax = plt.figure(figsize=(10,8)).add_subplot(111)
ax.plot( O3_global * 1.E6, np.log(lev/const.ps) )
ax.invert_yaxis()
ax.set_xlabel('Ozone (ppm)', fontsize=16)
ax.set_ylabel('Pressure (hPa)', fontsize=16 )
yticks = np.array([1000., 500., 250., 100., 50., 20., 10., 5.])
ax.set_yticks( np.log(yticks/1000.) )
ax.set_yticklabels( yticks )
ax.set_title('Global, annual mean ozone concentration', fontsize = 24);
```

This shows that most of the ozone is indeed in the stratosphere, and peaks near the top of the stratosphere.

Now create a new column model object **on the same pressure levels as the ozone data**.  We are also going set an adjusted lapse rate of 6 K / km, and tune the longwave absorption

```{code-cell}
oz_col = climlab.RadiativeConvectiveModel(lev = lev, abs_coeff=1.82E-4, adj_lapse_rate=6, albedo=0.315)
```

Now we will do something new: let the column absorb some shortwave radiation. We will assume that the shortwave absorptivity is proportional to the ozone concentration we plotted above.  We need to weight the absorptivity by the pressure (mass) of each layer.

```{code-cell}
ozonefactor = 75
dp = oz_col.Tatm.domain.axes['lev'].delta
sw_abs = O3_global * dp * ozonefactor
oz_col.subprocess.SW.absorptivity = sw_abs
oz_col.compute()
oz_col.compute()
print(oz_col.SW_absorbed_atm)
```

Now run it out to Radiative-Convective Equilibrium, and plot

```{code-cell}
oz_col.integrate_years(2.)

print("After " + str(oz_col.time['days_elapsed']) + " days of integration:")
print("Surface temperature is " + str(oz_col.Ts) + " K.")
print("Net energy in to the column is " + str(oz_col.ASR - oz_col.OLR) + " W / m2.")
```

```{code-cell}
pozcol = oz_col.lev

fig = plt.figure( figsize=(10,8) )
ax = fig.add_subplot(111)
ax.plot( Tglobal + const.tempCtoK, np.log(level/1000), 'b-', col.Tatm, np.log( pcol/const.ps ), 'r-' )
ax.plot( col.Ts, 0, 'ro', markersize=16 )
ax.plot( dalr_col.Tatm, np.log( pcol / const.ps ), 'k-' )
ax.plot( dalr_col.Ts, 0, 'ko', markersize=16 )
ax.plot( rce_col.Tatm, np.log( pcol / const.ps ), 'm-' )
ax.plot( rce_col.Ts, 0, 'mo', markersize=16 )
ax.plot( oz_col.Tatm, np.log( pozcol / const.ps ), 'c-' )
ax.plot( oz_col.Ts, 0, 'co', markersize=16 )
ax.invert_yaxis()
ax.set_xlabel('Temperature (K)', fontsize=16)
ax.set_ylabel('Pressure (hPa)', fontsize=16 )
ax.set_yticks( np.log(level/1000) )
ax.set_yticklabels( level )
ax.set_title('Temperature profiles: observed (blue), RE (red), dry RCE (black), moist RCE (magenta), RCE with ozone (cyan)', fontsize = 18)
ax.grid()
```

And we finally have something that looks looks like the tropopause, with temperature increasing above at about the correct rate. Though the tropopause temperature is off by 15 degrees or so.

+++

## Greenhouse warming in the RCE model with ozone

```{code-cell}
oz_col2 = climlab.process_like( oz_col )
```

```{code-cell}
oz_col2.subprocess['LW'].absorptivity *= 1.2 
```

```{code-cell}
oz_col2.integrate_years(2.)
```

```{code-cell}
fig = plt.figure( figsize=(10,8) )
ax = fig.add_subplot(111)
ax.plot( Tglobal + const.tempCtoK, np.log(level/const.ps), 'b-' )
ax.plot( oz_col.Tatm, np.log( pozcol / const.ps ), 'c-' )
ax.plot( oz_col.Ts, 0, 'co', markersize=16 )
ax.plot( oz_col2.Tatm, np.log( pozcol / const.ps ), 'c--' )
ax.plot( oz_col2.Ts, 0, 'co', markersize=16 )
ax.invert_yaxis()
ax.set_xlabel('Temperature (K)', fontsize=16)
ax.set_ylabel('Pressure (hPa)', fontsize=16 )
ax.set_yticks( np.log(level/const.ps) )
ax.set_yticklabels( level )
ax.set_title('Temperature profiles: observed (blue), RCE with ozone (cyan)', fontsize = 18)
ax.grid()
```

And we find that the troposphere warms, while the stratosphere cools!

+++

### Vertical structure of greenhouse warming in CESM model

```{code-cell}
atmstr = ".cam.h0.clim.nc"
cesm_ctrl = nc.Dataset( datapath + 'som_control/som_control' + atmstr + endstr )
cesm_2xCO2 = nc.Dataset( datapath + 'som_2xCO2/som_2xCO2' + atmstr + endstr )
```

```{code-cell}
T_cesm_ctrl = cesm_ctrl.variables['T'][:]
T_cesm_2xCO2 = cesm_2xCO2.variables['T'][:]
print(T_cesm_ctrl.shape)
```

```{code-cell}
T_cesm_ctrl_zon = np.mean( T_cesm_ctrl, axis=(0,3) )
T_cesm_2xCO2_zon = np.mean( T_cesm_2xCO2, axis=(0,3) )
```

```{code-cell}
T_cesm_2xCO2_glob = np.empty_like( lev )
T_cesm_ctrl_glob = np.empty_like( lev )
for n in range( lev.size ):
    T_cesm_ctrl_glob[n] = np.sum( T_cesm_ctrl_zon[n,:] * np.cos( np.deg2rad( lat ) ) ) / np.sum( np.cos( np.deg2rad( lat ) ) )
    T_cesm_2xCO2_glob[n] = np.sum( T_cesm_2xCO2_zon[n,:] * np.cos( np.deg2rad( lat ) ) ) / np.sum( np.cos( np.deg2rad( lat ) ) )
```

```{code-cell}
fig = plt.figure( figsize=(10,8) )
ax = fig.add_subplot(111)
ax.plot( Tglobal + const.tempCtoK, np.log(level/const.ps), 'b-' )
ax.plot( oz_col.Tatm, np.log( pozcol / const.ps ), 'c-' )
ax.plot( oz_col.Ts, 0, 'co', markersize=16 )
ax.plot( oz_col2.Tatm, np.log( pozcol / const.ps ), 'c--' )
ax.plot( oz_col2.Ts, 0, 'co', markersize=16 )
ax.plot( T_cesm_ctrl_glob, np.log( lev/const.ps ), 'r-' )
ax.plot( T_cesm_2xCO2_glob, np.log( lev/const.ps ), 'r--' )
ax.invert_yaxis()
ax.set_xlabel('Temperature (K)', fontsize=16)
ax.set_ylabel('Pressure (hPa)', fontsize=16 )
ax.set_yticks( np.log(level/const.ps) )
ax.set_yticklabels( level )
ax.set_title('Temperature profiles: observed (blue), RCE with ozone (cyan), CESM (red)', fontsize = 18)
ax.grid()
```

And we find that CESM has the same tendency for increased CO2: warmer troposphere, colder stratosphere.

```{code-cell}

```
