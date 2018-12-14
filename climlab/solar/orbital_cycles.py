from __future__ import division
from __future__ import print_function
from builtins import str
from builtins import range
from builtins import object
import numpy as np
from climlab import constants as const
from climlab.solar.orbital import OrbitalTable
from climlab.domain.field import global_mean


class OrbitalCycles(object):
    def __init__(self,
                 model,
                 kyear_start=-20.,
                 kyear_stop=0.,
                 segment_length_years=100.,
                 orbital_year_factor=1.,
                 verbose=True ):
        """Automatically integrates a process through changes in orbital parameters.

        OrbitalCycles is a module for setting up long integrations of climlab
        processes over orbital cycles.

        The duration between integration start and end time is partitioned in
        time segments over which the orbital parameters are held constant.
        The process is integrated over every time segment and the process state
        ``Ts`` is stored for each segment.

        The storage arrays are saving:

            * **current model state** at end of each segment
            * **model state averaged** over last integrated year of each segment
            * **global mean** of averaged model state over last integrated year
              of each segment

        .. note::

            Input ``kyear`` is thousands of years after present.
            For years before present, use ``kyear < 0``.


        **Initialization parameters** \n

        :param model:                       a time dependent process
        :type model:                        :class:`~climlab.process.time_dependent_process.TimeDependentProcess`
        :param float kyear_start:           integration start time.             \n
                                            As time reference
                                            is present, argument should be :math:`<0`
                                            for time before present.

                                                * *unit:* kiloyears        \n
                                                * *default value:* ``-20.``

        :param float kyear_stop:            integration stop time.             \n
                                            As time reference
                                            is present, argument should be :math:`\\le 0`
                                            for time before present.

                                                * *unit:* kiloyears        \n
                                                * *default value:* ``0.``

        :param float segment_length_years:  is the length of each integration with
                                            fixed orbital parameters. [default: 100.]
        :param float orbital_year_factor:   is an optional speed-up to the orbital cycles.
                                            [default: 1.]
        :param bool verbose:                prints product of calculation and
                                            information about computation progress
                                            [default: True]

        **Object attributes** \n

        Following object attributes are generated during initialization:

        :ivar model:                        timedependent process to be integrated
        :vartype model:                     :class:`~climlab.process.time_dependent_process.TimeDependentProcess`
        :ivar float kyear_start:            integration start time
        :ivar float kyear_stop:             integration stop time
        :ivar float segment_length_years:   length of each integration with
                                            fixed orbital parameters
        :ivar float orbital_year_factor:    speed-up factor  to the orbital cycles
        :ivar bool verbose:                 print flag
        :ivar int num_segments:             number of segments with fixed oribtal
                                            parameters, calculated through:

                                            .. math::

                                                num_{seg} = \\frac{-(kyear_{start}-kyear_{stop})*1000}{seg_{length} * orb_{factor}}

        :ivar array T_segments_global:      storage for global mean temperature
                                            for final year of each segment
        :ivar array T_segments:             storage for actual temperature at end
                                            of each segment
        :ivar array T_segments_annual:      storage for timeaveraged temperature
                                            over last year of segment \n
                                            dimension: (size(Ts), num_segments)
        :ivar array orb_kyear:              integration start time of all segments
        :ivar dict orb:                     orbital parameters for last integrated segment

        :Example:

            Integration of an energy balance model for 10,000 years with
            corresponding orbital parameters::

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
                experiment = OrbitalCycles(ebm, kyear_start=-20, kyear_stop=-10,
                                                        orbital_year_factor=10.)

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
        #orbtable = OrbitalTable()

        for n in range(self.num_segments):
            if verbose:
                print("-------------------------")
                print("Segment " + str(n) + " out of " + str(self.num_segments) )
                print( "Using orbital parameters from " + str(kyear_before_present) + " kyears before present." )
            self.orb = OrbitalTable.interp(kyear=kyear_before_present)
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
