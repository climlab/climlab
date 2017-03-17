import _mcica_subcol_gen_lw
import f90wrap.runtime
import logging

class Mcica_Subcol_Gen_Lw(f90wrap.runtime.FortranModule):
    """
    Module mcica_subcol_gen_lw
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 7-563
    
    """
    @staticmethod
    def mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, play, cldfrac, \
        ciwp, clwp, rei, rel, tauc, cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, \
        taucmcl):
        """
        mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, play, cldfrac, ciwp, \
            clwp, rei, rel, tauc, cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 50-155
        
        Parameters
        ----------
        iplon : int
        ncol : int
        nlay : int
        icld : int
        permuteseed : int
        irng : int
        play : float array
        cldfrac : float array
        ciwp : float array
        clwp : float array
        rei : float array
        rel : float array
        tauc : float array
        cldfmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
        taucmcl : float array
        
        """
        _mcica_subcol_gen_lw.f90wrap_mcica_subcol_lw(iplon=iplon, ncol=ncol, nlay=nlay, \
            icld=icld, permuteseed=permuteseed, irng=irng, play=play, cldfrac=cldfrac, \
            ciwp=ciwp, clwp=clwp, rei=rei, rel=rel, tauc=tauc, cldfmcl=cldfmcl, \
            ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, reicmcl=reicmcl, relqmcl=relqmcl, \
            taucmcl=taucmcl)
    
    @staticmethod
    def generate_stochastic_clouds(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, \
        ciwp, tauc, cld_stoch, clwp_stoch, ciwp_stoch, tauc_stoch, changeseed=None):
        """
        generate_stochastic_clouds(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, \
            ciwp, tauc, cld_stoch, clwp_stoch, ciwp_stoch, tauc_stoch[, changeseed])
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 159-523
        
        Parameters
        ----------
        ncol : int
        nlay : int
        nsubcol : int
        icld : int
        irng : int
        pmid : float array
        cld : float array
        clwp : float array
        ciwp : float array
        tauc : float array
        cld_stoch : float array
        clwp_stoch : float array
        ciwp_stoch : float array
        tauc_stoch : float array
        changeseed : int
        
        \
            -------------------------------------------------------------------------------------------------
        \
            ----------------------------------------------------------------------------------------------------------------
         ---------------------
         Contact: Cecile Hannay (hannay@ucar.edu)
         Original code: Based on Raisanen et al., QJRMS, 2004.
         Modifications: Generalized for use with RRTMG and added Mersenne Twister as the \
             default
           random number generator, which can be changed to the optional kissvec random \
               number generator
           with flag 'irng'. Some extra functionality has been commented or removed.
           Michael J. Iacono, AER, Inc., February 2007
         Given a profile of cloud fraction, cloud water and cloud ice, we produce a set \
             of subcolumns.
         Each layer within each subcolumn is homogeneous, with cloud fraction equal to \
             zero or one
         and uniform cloud liquid and cloud ice concentration.
         The ensemble as a whole reproduces the probability function of cloud liquid and \
             ice within each layer
         and obeys an overlap assumption in the vertical.
         Overlap assumption:
          The cloud are consistent with 4 overlap assumptions: random, maximum, \
              maximum-random and exponential.
          The default option is maximum-random (option 3)
          The options are: 1=random overlap, 2=max/random, 3=maximum overlap, \
              4=exponential overlap
          This is set with the variable "overlap"
        mji - Exponential overlap option (overlap=4) has been deactivated in this \
            version
          The exponential overlap uses also a length scale, Zo. (real, parameter :: Zo = \
              2500. )
         Seed:
          If the stochastic cloud generator is called several times during the same \
              timestep,
          one should change the seed between the call to insure that the subcolumns are \
              different.
          This is done by changing the argument 'changeSeed'
          For example, if one wants to create a set of columns for the shortwave and \
              another set for the longwave ,
          use 'changeSeed = 1' for the first call and'changeSeed = 2' for the second call
         PDF assumption:
          We can use arbitrary complicated PDFS.
          In the present version, we produce homogeneuous clouds (the simplest case).
          Future developments include using the PDF scheme of Ben Johnson.
         History file:
          Option to add diagnostics variables in the history file. (using FINCL in the \
              namelist)
          nsubcol = number of subcolumns
          overlap = overlap type (1-3)
          Zo = length scale
          CLOUD_S = mean of the subcolumn cloud fraction ('_S" means Stochastic)
          CLDLIQ_S = mean of the subcolumn cloud water
          CLDICE_S = mean of the subcolumn cloud ice
         Note:
           Here: we force that the cloud condensate to be consistent with the cloud \
               fraction
           i.e we only have cloud condensate when the cell is cloudy.
           In CAM: The cloud condensate and the cloud fraction are obtained from 2 \
               different equations
           and the 2 quantities can be inconsistent (i.e. CAM can produce cloud fraction
           without cloud condensate or the opposite).
        \
            ---------------------------------------------------------------------------------------------------------------
        """
        _mcica_subcol_gen_lw.f90wrap_generate_stochastic_clouds(ncol=ncol, nlay=nlay, \
            nsubcol=nsubcol, icld=icld, irng=irng, pmid=pmid, cld=cld, clwp=clwp, \
            ciwp=ciwp, tauc=tauc, cld_stoch=cld_stoch, clwp_stoch=clwp_stoch, \
            ciwp_stoch=ciwp_stoch, tauc_stoch=tauc_stoch, changeseed=changeseed)
    
    @staticmethod
    def kissvec(seed1, seed2, seed3, seed4, ran_arr):
        """
        kissvec(seed1, seed2, seed3, seed4, ran_arr)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 530-562
        
        Parameters
        ----------
        seed1 : int array
        seed2 : int array
        seed3 : int array
        seed4 : int array
        ran_arr : float array
        
        \
            --------------------------------------------------------------------------------------------------
         public domain code
         made available from http://www.fortran.com/
         downloaded by pjr on 03/16/04 for NCAR CAM
         converted to vector form, functions inlined by pjr,mvr on 05/10/2004
         The  KISS (Keep It Simple Stupid) random number generator. Combines:
         (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
         (2) A 3-shift shift-register generator, period 2^32-1,
         (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
          Overall period>2^123;
        """
        _mcica_subcol_gen_lw.f90wrap_kissvec(seed1=seed1, seed2=seed2, seed3=seed3, \
            seed4=seed4, ran_arr=ran_arr)
    
    _dt_array_initialisers = []
    

mcica_subcol_gen_lw = Mcica_Subcol_Gen_Lw()

