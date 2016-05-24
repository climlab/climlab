
import climlab
from climlab.dynamics.budyko_transport import BudykoTransport
from climlab import domain
from climlab.domain import field
from climlab.utils.legendre import P2
import numpy as np
import matplotlib.pyplot as plt

# create domain
sfc = domain.zonal_mean_surface(num_lat = 36)

lat = sfc.lat.points
lat_rad = np.deg2rad(lat)

# define initial temperature distribution
T0 = 15.
T2 = -20.
Ts = field.Field(T0 + T2 * P2(np.sin(lat_rad)), domain=sfc)

# create BudykoTransport process
budyko_transp = BudykoTransport(state=Ts)

### Integrate & Plot ###

fig = plt.figure( figsize=(6,4))
ax = fig.add_subplot(111)

for i in np.arange(0,3,1):  
    ax.plot(lat, budyko_transp.default, label='day %s' % (i*40))
    budyko_transp.integrate_days(40.)

ax.set_title('Standalone Budyko Transport')
ax.set_xlabel('latitude')
ax.set_xticks([-90,-60,-30,0,30,60,90])
ax.set_ylabel('temperature ($^{\circ}$C)')
ax.legend(loc='best')
plt.show()
