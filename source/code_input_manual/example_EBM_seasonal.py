import climlab
from climlab.utils import constants as const
import numpy as np
import matplotlib.pyplot as plt

# creating model
model = climlab.EBM_seasonal()
model.step_forward()

solar = model.subprocess['insolation'].insolation

# plot
fig = plt.figure( figsize=(6,4))
ax = fig.add_subplot(111)

season_days = const.days_per_year/4

for season in ['winter','spring','summer','autumn']:
    ax.plot(model.lat, solar, label=season)
    model.integrate_days(season_days)

ax.set_title('seasonal solar distribution')
ax.set_xlabel('latitude')
ax.set_xticks([-90,-60,-30,0,30,60,90])
ax.set_ylabel('solar insolation (W/m$^2$)')
ax.legend(loc='best')
plt.show()
