import climlab
from climlab.dynamics.diffusion import Diffusion
import matplotlib.pyplot as plt

c = climlab.GreyRadiationModel()
K = 0.5
d = Diffusion(K=K, state = {'Tatm':c.state['Tatm']}, **c.param)

c.add_subprocess('diffusion',d)

### Integrate & Plot ###

fig = plt.figure( figsize=(6,4))
ax = fig.add_subplot(111)

ax.plot(c.lev, c.state['Tatm'], label='step 0')
c.step_forward()
ax.plot(c.lev, c.state['Tatm'], label='step 1')

ax.invert_xaxis()
ax.set_title('Diffusion subprocess')
ax.set_xlabel('level (mb)')
#ax.set_xticks([])
ax.set_ylabel('temperature (K)')
ax.legend(loc='best')
plt.show()
