import numpy as np
import climlab
from climlab.dynamics.diffusion import MeridionalDiffusion
from climlab.utils import legendre

sfc = climlab.domain.zonal_mean_surface(num_lat=90, water_depth=10.)
lat = sfc.lat.points
initial = 12. - 40. * legendre.P2(np.sin(np.deg2rad(lat)))

# make a copy of initial so that it remains unmodified
Ts = climlab.Field(np.array(initial), domain=sfc)

# thermal diffusivity in W/m**2/degC
D = 0.55

# meridional diffusivity in 1/s
K = D / sfc.heat_capacity
d = MeridionalDiffusion(state=Ts, K=K)

d.integrate_years(1.)

import matplotlib.pyplot as plt

fig = plt.figure( figsize=(6,4))
ax = fig.add_subplot(111)
ax.set_title('Example for Meridional Diffusion')
ax.set_xlabel('latitude')
ax.set_xticks([-90,-60,-30,0,30,60,90])
ax.set_ylabel('temperature ($^{\circ}$C)')
ax.plot(lat, initial, 	label='initial')
ax.plot(lat, Ts, label='Ts (1yr)')
ax.legend(loc='best')
plt.show()


