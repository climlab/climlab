---
jupytext:
  encoding: '# -*- coding: utf-8 -*-'
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

# The seasonal cycle of surface temperature

+++

Look at the observed seasonal cycle in the NCEP reanalysis data.

Read in the necessary data from the online server.

The catalog is here: <http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis.derived/catalog.html>

```{code-cell}
from __future__ import division, print_function
%matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
import netCDF4 as nc
import climlab
from climlab.solar.insolation import daily_insolation
from climlab import constants as const

datapath = "http://ramadda.atmos.albany.edu:8080/repository/opendap/latest/Top/Users/BrianRose/CESM_runs/"
endstr = "/entry.das"
```

```{code-cell}
ncep_url = "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis.derived/"
ncep_air = nc.Dataset( ncep_url + "pressure/air.mon.1981-2010.ltm.nc" )
ncep_Ts = nc.Dataset( ncep_url + "surface_gauss/skt.sfc.mon.1981-2010.ltm.nc" )
lat_ncep = ncep_Ts.variables['lat'][:]; lon_ncep = ncep_Ts.variables['lon'][:]
Ts_ncep = ncep_Ts.variables['skt'][:]
print(Ts_ncep.shape)
```

Load the topography file from CESM, just so we can plot the continents.

```{code-cell}
topo = nc.Dataset( datapath + 'som_input/USGS-gtopo30_1.9x2.5_remap_c050602.nc' + endstr )
lat_cesm = topo.variables['lat'][:]
lon_cesm = topo.variables['lon'][:]
```

Make two maps: one of annual mean surface temperature, another of the seasonal range (max minus min).

```{code-cell}
maxTs = np.max(Ts_ncep,axis=0)
minTs = np.min(Ts_ncep,axis=0)
```

```{code-cell}
fig = plt.figure( figsize=(16,6) )

ax1 = fig.add_subplot(1,2,1)
cax1 = ax1.pcolormesh( lon_ncep, lat_ncep, np.mean(Ts_ncep, axis=0), cmap=plt.cm.seismic )
cbar1 = plt.colorbar(cax1)
ax1.set_title('Annual mean surface temperature (degC)', fontsize=14 )

ax2 = fig.add_subplot(1,2,2)
cax2 = ax2.pcolormesh( lon_ncep, lat_ncep, maxTs - minTs )
cbar2 = plt.colorbar(cax2)
ax2.set_title('Seasonal temperature range (degC)', fontsize=14)

for ax in [ax1,ax2]:
    ax.contour( lon_cesm, lat_cesm, topo.variables['LANDFRAC'][:], levels=[0.5], colors='k');
    ax.axis([0, 360, -90, 90])
    ax.set_xlabel('Longitude', fontsize=14 ); ax.set_ylabel('Latitude', fontsize=14 )
```

Make a contour plot of the zonal mean temperature as a function of time of year

```{code-cell}
[-65,-55,-45,-35,-25,-15,-5,5,15,25,35,45,55,65]
```

```{code-cell}
np.arange(-65,75,10)
```

```{code-cell}
Tmax = 65; Tmin = -Tmax; delT = 10
clevels = np.arange(Tmin,Tmax+delT,delT)

fig_zonobs = plt.figure( figsize=(10,6) )
ax = fig_zonobs.add_subplot(111)
cax = ax.contourf( np.arange(12)+0.5, lat_ncep, np.transpose(np.mean( Ts_ncep, axis=2 )), levels=clevels, 
               cmap=plt.cm.seismic, vmin=Tmin, vmax=Tmax )
ax.set_xlabel('Month', fontsize=16)
ax.set_ylabel('Latitude', fontsize=16 )
cbar = plt.colorbar(cax)
ax.set_title('Zonal mean surface temperature (degC)', fontsize=20)
plt.show()
```

## Exploring the amplitude of the seasonal cycle with an EBM

+++

We are looking at the 1D (zonally averaged) energy balance model with diffusive heat transport. The equation is

+++

$C \frac{\partial T(\phi,t)}{\partial t} = \big(1-\alpha\big) Q(\phi,t) - \Big(A+B T(\phi,t) \Big) + 
\frac{K}{\cos\phi} \frac{\partial}{\partial \phi} \bigg( \cos\phi \frac{\partial T}{\partial \phi} \bigg)$

+++

and the code in `climlab.EBM` solves this equation numerically.

+++

One handy feature of `climlab` process code: the function `integrate_years()` automatically calculates the time averaged temperature. So if we run it for exactly one year, we get the annual mean temperature saved in the field `T_timeave`.

+++

We will look at the seasonal cycle of temperature in three different models with different heat capacities (which we express through an equivalent depth of water in meters):

```{code-cell}
model1 = climlab.EBM_seasonal()
model1.integrate_years(1, verbose=True)

water_depths = np.array([2., 10., 50.])

num_depths = water_depths.size
Tann = np.empty( [model1.lat.size, num_depths] )
models = []

for n in range(num_depths):
    models.append(climlab.EBM_seasonal(water_depth=water_depths[n]))
    models[n].integrate_years(20., verbose=False )
    models[n].integrate_years(1., verbose=False)
    Tann[:,n] = np.squeeze(models[n].timeave['Ts'])
```

All models should have the same annual mean temperature:

```{code-cell}
lat = model1.lat

plt.plot(lat, Tann)
plt.xlim(-90,90)
plt.xlabel('Latitude')
plt.ylabel('Temperature (degC)')
plt.title('Annual mean temperature in the EBM')
plt.legend( water_depths.astype(str) )
plt.show()
```

There is no automatic function in `climlab.EBM` to keep track of minimum and maximum temperatures (though we might add that in the future!)

Instead we'll step through one year "by hand" and save all the temperatures.

```{code-cell}
num_steps_per_year = int(model1.time['num_steps_per_year'])
Tyear = np.empty((lat.size, num_steps_per_year, num_depths))
for n in range(num_depths):
    for m in range(num_steps_per_year):
        models[n].step_forward()
        Tyear[:,m,n] = np.squeeze(models[n].Ts)
```

Make a figure to compare the observed zonal mean seasonal temperature cycle to what we get from the EBM with different heat capacities:

```{code-cell}
fig = plt.figure( figsize=(20,10) )

ax = fig.add_subplot(2,num_depths,2)
cax = ax.contourf( np.arange(12)+0.5, lat_ncep, np.transpose(np.mean( Ts_ncep, axis=2 )), levels=clevels, 
               cmap=plt.cm.seismic, vmin=Tmin, vmax=Tmax )
ax.set_xlabel('Month')
ax.set_ylabel('Latitude')
cbar = plt.colorbar(cax)
ax.set_title('Zonal mean surface temperature - observed (degC)', fontsize=20)

for n in range(num_depths):
    ax = fig.add_subplot(2,num_depths,num_depths+n+1)
    cax = ax.contourf( 4*np.arange(num_steps_per_year), lat, Tyear[:,:,n], levels=clevels, 
               cmap=plt.cm.seismic, vmin=Tmin, vmax=Tmax )
    cbar1 = plt.colorbar(cax)
    ax.set_title('water_depth = ' + str(models[n].param['water_depth']) + ' meters', fontsize=20 )
    ax.set_xlabel('Days of year', fontsize=14 )
    ax.set_ylabel('Latitude', fontsize=14 )

#fig.set_title('Temperature in seasonal EBM with various water depths', fontsize=14)
plt.show()
```

Which one looks more realistic? Depends a bit on where you look. But overall, the observed seasonal cycle matches the 10 meter case best. The effective heat capacity governing the seasonal cycle of the zonal mean temperature is closer to 10 meters of water than to either 2 or 50 meters.

+++

### Making an animation of the EBM solutions

```{code-cell}
fpath = '/Users/Brian/Dropbox/PythonStuff/ebm_seasonal_frames/'

fig = plt.figure( figsize=(20,5) )
#for m in range(model2.time['num_steps_per_year']):
for m in range(1):
    thisday = m * models[0].param['timestep'] / const.seconds_per_day
    Q = daily_insolation( lat, thisday )
    for n in range(num_depths):
        c1 = 'b'
        ax = fig.add_subplot(1,num_depths,n+1)
        ax.plot( lat, Tyear[:,m,n], c1 )
        ax.set_title('water_depth = ' + str(models[n].param['water_depth']) + ' meters', fontsize=20 )
        ax.set_xlabel('Latitude', fontsize=14 )
        if n is 0:
            ax.set_ylabel('Temperature', fontsize=14, color=c1 )
        ax.set_xlim([-90,90])
        ax.set_ylim([-60,60])
        for tl in ax.get_yticklabels():
            tl.set_color(c1)
        ax.grid()
        
        c2 = 'r'
        ax2 = ax.twinx()
        ax2.plot( lat, Q, c2)
        if n is 2:
            ax2.set_ylabel('Insolation (W m$^{-2}$)', color=c2, fontsize=14)
        for tl in ax2.get_yticklabels():
            tl.set_color(c2)
        ax2.set_xlim([-90,90])
        ax2.set_ylim([0,600])
    
    filename = fpath + 'ebm_seasonal' + str(m).zfill(4) + '.png'
    #fig.savefig(filename)
    #fig.clear()
    plt.show()
```

## The seasonal cycle for a planet with 90º obliquity

+++

The EBM code uses our familiar `insolation.py` code to calculate insolation, and therefore it's easy to set up a model with different orbital parameters. Here is an example with **very** different orbital parameters: 90º obliquity. We looked at the distribution of insolation by latitude and season for this type of planet in the last homework.

```{code-cell}
orb_highobl = {'ecc':0., 'obliquity':90., 'long_peri':0.}
print(orb_highobl)
model_highobl = climlab.EBM_seasonal(orb=orb_highobl)
print(model_highobl.param['orb'])
```

Repeat the same procedure to calculate and store temperature throughout one year, after letting the models run out to equilibrium.

```{code-cell}
Tann_highobl = np.empty( [lat.size, num_depths] )
models_highobl = []

for n in range(num_depths):
    models_highobl.append(climlab.EBM_seasonal(water_depth=water_depths[n], orb=orb_highobl))
    models_highobl[n].integrate_years(40., verbose=False )
    models_highobl[n].integrate_years(1.)
    Tann_highobl[:,n] = np.squeeze(models_highobl[n].timeave['Ts'])

Tyear_highobl = np.empty([lat.size, num_steps_per_year, num_depths])
for n in range(num_depths):
    for m in range(num_steps_per_year):
        models_highobl[n].step_forward()
        Tyear_highobl[:,m,n] = np.squeeze(models_highobl[n].Ts)
```

And plot the seasonal temperature cycle same as we did above:

```{code-cell}
fig = plt.figure( figsize=(20,5) )
Tmax_highobl = 125; Tmin_highobl = -Tmax_highobl; delT_highobl = 10
clevels_highobl = np.arange(Tmin_highobl, Tmax_highobl+delT_highobl, delT_highobl)

for n in range(num_depths):
    ax = fig.add_subplot(1,num_depths,n+1)
    cax = ax.contourf( 4*np.arange(num_steps_per_year), lat, Tyear_highobl[:,:,n], 
        levels=clevels_highobl, cmap=plt.cm.seismic, vmin=Tmin_highobl, vmax=Tmax_highobl )
    cbar1 = plt.colorbar(cax)
    ax.set_title('water_depth = ' + str(models_highobl[n].param['water_depth']) + ' meters', fontsize=20 )
    ax.set_xlabel('Days of year', fontsize=14 )
    ax.set_ylabel('Latitude', fontsize=14 )

plt.show()
```

Note that the temperature range is much larger than for the Earth-like case above (but same contour interval, 10 degC).

+++

Why is the temperature so uniform in the north-south direction with 50 meters of water?

To see the reason, let's plot the annual mean insolation at 90º obliquity, alongside the present-day annual mean insolation:

```{code-cell}
lat2 = np.linspace(-90, 90, 181)
days = np.linspace(1.,50.)/50 * const.days_per_year
Q_present = daily_insolation( lat2, days )
Q_highobl = daily_insolation( lat2, days, orb_highobl )
Q_present_ann = np.mean( Q_present, axis=1 )
Q_highobl_ann = np.mean( Q_highobl, axis=1 )
```

```{code-cell}
plt.plot( lat2, Q_present_ann, label='Earth' )
plt.plot( lat2, Q_highobl_ann, label='90deg obliquity' )
plt.grid()
plt.legend(loc='lower center')
plt.xlabel('Latitude', fontsize=14 )
plt.ylabel('W m$^{-2}$', fontsize=14 )
plt.title('Annual mean insolation for two different obliquities', fontsize=16)
plt.show()
```

Though this is a bit misleading, because our model prescribes an increase in albedo from the equator to the pole. So the absorbed shortwave gradients look even more different.

```{code-cell}

```
