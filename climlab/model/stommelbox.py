## NEED TO FIX THE PASSING OF INITIAL PARAMETERS 
# especially timestep


# how easy is to implement the Stommel 1961 box model in climlab?
import numpy as np
from climlab.process.time_dependent_process import _TimeDependentProcess
import climlab.domain.domain as domain
import climlab.domain.field as field
box = domain.box_model_domain()
print box.shape

# initial condition
x = field.Field([1.,0.], domain=box)
y = field.Field([1.,1.], domain=box)
state = {'x':x, 'y':y}
# define the process
class StommelBox(_TimeDependentProcess):
    def compute(self):
        x = self.state['x']
        y = self.state['y']    
        dt = self.param['timestep']
        term = np.abs(-y + self.param['R']*x) / self.param['lam']
        self.tendencies['y'] = (1 - y - y * term)*dt
        self.tendencies['x'] = (self.param['delta'] * (1 - x) - x * term)*dt
# make a parameter dictionary
param = {'R': 2., 'lam': 1., 'delta': 1.}
# instantiate the process
boxmodel = StommelBox(state=state, param=param)
# change the timestep
boxmodel.set_timestep(num_steps_per_year=1E9)

