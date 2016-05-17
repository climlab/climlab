import climlab
import matplotlib.pyplot as plt

# creating & integrating model
model = climlab.EBM()
model.integrate_converge()

# plot
fig = plt.figure( figsize=(6,4))
ax = fig.add_subplot(111)

ax.plot(model.lat, model.heat_transport_convergence())

ax.set_title('heat transport convergence')
ax.set_xlabel('latitude')
ax.set_xticks([-90,-60,-30,0,30,60,90])
ax.set_ylabel('energy (W/m$^2$)')
plt.axhline(linewidth=2, color='grey', linestyle='dashed')
plt.show()
