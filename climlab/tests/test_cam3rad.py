import numpy as np
import climlab
import pytest

num_lev = 30
alb = 0.25

@pytest.fixture()
def rcm():
    # initial state (temperatures)
    state = climlab.column_state(num_lev=num_lev, num_lat=1, water_depth=5.)
    #  Create a parent process
    rcm = climlab.TimeDependentProcess(state=state)
    ## Create individual physical process models:
    #  fixed relative humidity
    h2o = climlab.radiation.water_vapor.ManabeWaterVapor(state=state)
    #  Hard convective adjustment
    convadj = climlab.convection.convadj.ConvectiveAdjustment(state=state,
                                                             adj_lapse_rate=6.5)
    # CAM3 radiation with default parameters and interactive water vapor
    rad = climlab.radiation.CAM3Radiation(state=state, O3init=True,
                                     aldif=alb, aldir=alb, asdif=alb, asdir=alb)
    rad.q = h2o.q
    # Couple the models
    rcm.add_subprocess('Radiation', rad)
    rcm.add_subprocess('ConvectiveAdjustment', convadj)
    rcm.add_subprocess('H2O', h2o)
    #  Make diagnostic water vapor field easy to access
    rcm.q = rcm.subprocess.H2O.q
    return rcm

#  This test is not useful anymore,
#the fortran object is not attached to the Process object
#def test_model_creation(rcm):
#    '''Just make sure we can create a model and have access to the Fortran object.'''
#    assert len(rcm.Tatm)==num_lev
#    assert (rcm.subprocess.Radiation.extension.get_nlev() == num_lev)

def test_rce(rcm):
    '''Test a single-column radiative-convective model with CAM3 radiation and
    fixed relative humidity.'''
    rcm.step_forward()
    #rcm.integrate_years(5)
    #assert(np.isclose(rcm.Ts, ))

def test_radiative_forcing(rcm):
    '''Run a single-column radiative-convective model with CAM3 radiation
    out to equilibrium. Clone the model, double CO2 and measure the instantaneous
    change in TOA flux. It should be positive net downward flux.'''
    rcm.integrate_years(5.)
    assert np.abs(rcm.subprocess['Radiation'].ASR - rcm.subprocess['Radiation'].OLR) < 0.1  # close to energy balance
    rcm2 = climlab.process_like(rcm)
    rcm2.subprocess['Radiation'].CO2 *= 2.
    rcm2.compute_diagnostics()
    assert (rcm2.subprocess['Radiation'].ASR - rcm2.subprocess['Radiation'].OLR) > 1.  # positive radiative forcing
