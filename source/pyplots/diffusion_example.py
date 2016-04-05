import climlab
from climlab.dynamics.diffusion import Diffusion

c = climlab.GreyRadiationModel()
K = 0.5
d = Diffusion(K=K, state = {'Tatm':c.state['Tatm']}, **c.param)

#c.subprocess['diffusion'] = d
c.add_subprocess('diffusion',d)

print c.state
print d.state

c.step_forward()

print c.state
print d.state
