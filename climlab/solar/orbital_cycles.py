'''Module for setting up long integrations of climlab processes
over orbital cycles

example usage:

from climlab.model.ebm import EBM_seasonal
from climlab.solar.orbital_cycles import OrbitalCycles
from climlab.surface.albedo import StepFunctionAlbedo
ebm = EBM_seasonal()
print ebm
#  add an albedo feedback
albedo = StepFunctionAlbedo(state=ebm.state, **ebm.param)
ebm.add_subprocess('albedo', albedo)
#  start the integration
#  run for 10,000 orbital years, but only 1,000 model years
experiment = OrbitalCycles(ebm, kyear_start=-20, 
                           kyear_stop=-10, orbital_year_factor=10.)

'''

import numpy as np
from climlab import constants as const
from climlab.solar.orbital import OrbitalTable
from climlab.domain.field import global_mean


class OrbitalCycles:
    def __init__(self,
                 model,
                 kyear_start=-20.,
                 kyear_stop=0.,
                 segment_length_years=100.,
                 orbital_year_factor=1.,
                 verbose=True ):
        """Automatically integrate a process through changes in orbital parameters.
        
        model is an instance of climlab.time_dependent_process
        
        segment_length_years is the length of each integration with fixed orbital parameters.
        orbital_year_factor is an optional speed-up to the orbital cycles.
        """
        
        self.model = model
        self.kyear_start = kyear_start
        self.kyear_stop = kyear_stop
        self.segment_length_years = segment_length_years
        self.orbital_year_factor = orbital_year_factor
        self.verbose = verbose
        self.num_segments = int(-(kyear_start - kyear_stop) * 1000. /
                                segment_length_years / orbital_year_factor)

        kyear_before_present = kyear_start

        if verbose:
            print("---------  OrbitalCycles  START ----------")
            print("Beginning integration for the model from " + str(kyear_start) + " to " + 
                str(kyear_stop) + " kyears before present.")
            print("Integration time for each set of orbital parameters is " + 
                str(segment_length_years) + " years.")
            print("Orbital cycles will be sped up by a factor " + str(orbital_year_factor))
            print("Total number of segments is " + str(self.num_segments))
        
        # initialize storage arrays
        self.T_segments_global = np.empty( self.num_segments )
        self.T_segments = np.empty( (self.model.Ts.size, self.num_segments) )
        self.T_segments_annual = np.empty_like( self.T_segments )
        self.orb_kyear = np.empty( self.num_segments )
        
        # Get orbital data table
        orbtable = OrbitalTable()
        
        for n in range(self.num_segments):
            if verbose:
                print("-------------------------")
                print("Segment " + str(n) + " out of " + str(self.num_segments) )
                print( "Using orbital parameters from " + str(kyear_before_present) + " kyears before present." )
            self.orb = orbtable.lookup_parameters(kyear_before_present)
            #self.model.make_insolation_array( orb )
            self.model.subprocess['insolation'].orb = self.orb
            self.model.integrate_years(segment_length_years-1., verbose=False)
            #  Run one final year to characterize the current equilibrated state
            self.model.integrate_years(1.0, verbose=False)
            self.T_segments_annual[:, n] = np.squeeze(self.model.timeave['Ts'])
            self.T_segments[:, n] = np.squeeze(self.model.Ts)
            self.T_segments_global[n] = global_mean(self.model.timeave['Ts'])
            self.orb_kyear[n] = kyear_before_present
            kyear_before_present += segment_length_years / 1000. * orbital_year_factor
            if verbose:
                print( "Global mean temperature from the final year of integration is " + 
                    str(self.T_segments_global[n]) + " degrees C." )
        if verbose:
            print("---------  OrbitalCycles  END ----------")
